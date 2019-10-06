#!/bin/bash

TERM=st

MODE=$1
PORT=$2

OCAML_VERSIONS="4.02.3 4.03.0 4.04.2 4.05.0"
# Increment whenever the OCaml version or a package is updated to invalidate the caches
SERIAL=1
# Set to 0 if the testsuite may fail
TESTSUITE_SOUND=0

ROOT=C:/OCaml
ROOT_CYG=$(echo $ROOT| cygpath -f -)
APPVEYOR_BUILD_FOLDER=$(echo $APPVEYOR_BUILD_FOLDER| cygpath -f -)

ERRORS_ALLOWED=0
function quietly_log {
  if ! script --quiet --return --append --command "$1" $LOG_FILE > /dev/null 2>&1 ; then
    cat $LOG_FILE
    if ((ERRORS_ALLOWED)) ; then
      return 1
    else
      exit 1
    fi
  fi
}

function msvs_promote_path {
  if [[ ${1%64} = "msvc" ]] ; then
    eval $($ROOT_CYG/msvs-promote-path)
  fi
}

case $MODE in
  prepare)
    BUILT_SOMETHING=0
    for OCAML_VERSION in $OCAML_VERSIONS ; do
      OCAML_BRANCH=${OCAML_VERSION%.*}
      if ! cat $APPVEYOR_BUILD_FOLDER/appveyor.yml | tr -d '\015' | sed -e '1,/^cache:/d' -e '/^$/,$d' | grep -q "^ \+- \+C:\\\\OCaml\\\\$OCAML_BRANCH$" ; then
        echo "$(tput setf 4)ERROR$(tput sgr0) C:\\OCaml\\$OCAML_BRANCH doesn't appear to be cached in appveyor.yml"
        exit 1
      fi

      if [[ ! -e $ROOT_CYG/$OCAML_BRANCH/$PORT/bin/ocamlopt.exe || ! -e $ROOT_CYG/$OCAML_BRANCH/version || $(cat $ROOT_CYG/$OCAML_BRANCH/version) != "$OCAML_VERSION-$SERIAL" ]] ; then
        if [[ -e $ROOT_CYG/$OCAML_BRANCH/version && $(cat $ROOT_CYG/$OCAML_BRANCH/version) != "$OCAML_VERSION-$SERIAL" ]] ; then
          echo "Build cache for $OCAML_BRANCH has serial $(cat $ROOT_CYG/$OCAML_BRANCH/version); should be $OCAML_VERSION-$SERIAL -- clearing"
          rm -rf $ROOT_CYG/$OCAML_BRANCH
        elif [[ ! -e $ROOT_CYG/$OCAML_BRANCH/version ]] ; then
          rm -rf $ROOT_CYG/$OCAML_BRANCH
        fi

        if ((BUILT_SOMETHING)) ; then
          if [[ $PORT = "mingw" ]] ; then
            appveyor AddMessage "OCaml $OCAML_VERSION needs to be built, but this run has already built a compiler set." -Detail "Assuming the build completes successfully, use the Re-build Commit option" -Category Warning
          fi
        else
          PREFIX=$ROOT_CYG/$OCAML_BRANCH/$PORT
          ROOT=$ROOT/$OCAML_BRANCH/$PORT
          OCAML_BRANCH=${OCAML_BRANCH/.}

          if [[ ! -d $APPVEYOR_BUILD_FOLDER/../src ]] ; then
            mkdir -p $APPVEYOR_BUILD_FOLDER/../src
            cd $APPVEYOR_BUILD_FOLDER/../src
            git clone https://github.com/ocaml/ocaml.git
            cd ocaml
            mkdir -p $PREFIX
            cp tools/msvs-promote-path $ROOT_CYG/
            cd ..
            appveyor DownloadFile "http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip" -FileName flexdll-bin-0.35.zip
            appveyor DownloadFile "https://github.com/mjambon/biniou/archive/v1.2.0.tar.gz" -FileName biniou-1.2.0.tar.gz
            appveyor DownloadFile "https://github.com/mjambon/cppo/archive/v1.5.0.tar.gz" -FileName cppo-1.5.0.tar.gz
            appveyor DownloadFile "https://github.com/mjambon/easy-format/archive/v1.2.0.tar.gz" -FileName easy-format-1.2.0.tar.gz
            appveyor DownloadFile "http://download.camlcity.org/download/findlib-1.7.3.tar.gz" -FileName findlib-1.7.3.tar.gz
            appveyor DownloadFile "https://github.com/ocaml/dune/releases/download/1.0.1/dune-1.0.1.tbz" -FileName dune-1.0.1.tbz
            appveyor DownloadFile "https://github.com/ocaml/ocamlbuild/archive/0.11.0.tar.gz" -FileName ocamlbuild-0.11.0.tar.gz
            appveyor DownloadFile "https://github.com/ocaml-community/yojson/archive/v1.6.0.tar.gz" -FileName yojson-1.6.0.tar.gz
            cp $APPVEYOR_BUILD_FOLDER/appveyor/*.patch $APPVEYOR_BUILD_FOLDER/../src/
            [[ -e $PREFIX/../version ]] || echo $OCAML_VERSION-$SERIAL> $PREFIX/../version
          fi

          export PATH=$PREFIX/bin:$PATH

          cd $APPVEYOR_BUILD_FOLDER/../src/ocaml
          git checkout $OCAML_VERSION
          git worktree add ../$OCAML_VERSION/$PORT/ocaml -b build-$OCAML_VERSION-$PORT
          if [[ $OCAML_BRANCH -ge 403 ]] ; then
            pushd ../$OCAML_VERSION/$PORT/ocaml
            git submodule update --init
            popd
          fi
          cd ../$OCAML_VERSION/$PORT/ocaml
          if [[ $OCAML_BRANCH -ge 406 ]] ; then
            cp config/s-nt.h byterun/caml/s.h
            cp config/m-nt.h byterun/caml/m.h
          else
            cp config/s-nt.h config/s.h
            cp config/m-nt.h config/m.h
          fi
          if [[ $OCAML_BRANCH -ge 405 ]] ; then
            POST_WORLD=flexlink.opt
          else
            POST_WORLD=
          fi
          if [[ $OCAML_BRANCH -lt 403 ]] ; then
            mkdir -p $PREFIX/bin
            pushd $PREFIX/bin
            case $PORT in
              msvc)
                MANIFEST=default.manifest;;
              msvc64)
                MANIFEST=default_amd64.manifest;;
              *)
                MANIFEST=;;
            esac
            unzip $APPVEYOR_BUILD_FOLDER/../src/flexdll-bin-0.35.zip flexdll_*$PORT.* flexdll.h flexlink.exe $MANIFEST
            popd
            PRE_WORLD=
          else
            PRE_WORLD=flexdll
          fi
          sed -e "s|PREFIX=[^\r]*|PREFIX=$ROOT|" config/Makefile.$PORT > config/Makefile
          msvs_promote_path $PORT
          cd ..
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/findlib-1.7.3.tar.gz
          cd findlib-1.7.3
          # Upstreamed; not merged
          patch -p1 -i $APPVEYOR_BUILD_FOLDER/../src/findlib-1.7.3.patch
          # Not yet upstreamed
          sed -i -e 's/\.a/$(LIB_SUFFIX)/g' src/findlib/Makefile
          cd ..
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/dune-1.0.1.tbz
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/easy-format-1.2.0.tar.gz
          cd easy-format-1.2.0
          # Upstreaming not required: master has been converted to jbuilder
          patch -p1 -i $APPVEYOR_BUILD_FOLDER/../src/easy-format-1.2.0.patch
          cd ..
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/biniou-1.2.0.tar.gz
          if [[ $OCAML_BRANCH -ge 403 ]] ; then
            tar -xzf $APPVEYOR_BUILD_FOLDER/../src/ocamlbuild-0.11.0.tar.gz
            cd ocamlbuild-0.11.0
            # Manually apply fix from a8d2e8
            sed -i -e 's/pack\.o/pack$(EXT_OBJ)/g' Makefile
            cd ..
          fi
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/cppo-1.5.0.tar.gz
          tar -xzf $APPVEYOR_BUILD_FOLDER/../src/yojson-1.6.0.tar.gz
          cd ocaml

          LOG_FILE=OCaml-$OCAML_VERSION-$PORT.log
          echo "Building OCaml $OCAML_VERSION for $PORT" | tee $LOG_FILE
          echo "Please see $LOG_FILE for further information"
          LOG_FILE="$APPVEYOR_BUILD_FOLDER/$LOG_FILE"
          quietly_log "make -f Makefile.nt $PRE_WORLD world.opt $POST_WORLD install"
          # Remove unnecessary executables to keep the build cache size down
          # These are removed here to ensure findlib doesn't configure itself
          # to use .opt commands
          if [[ $OCAML_BRANCH -ge 404 ]] ; then
            rm $PREFIX/bin/*.byte.exe  $PREFIX/bin/*.opt.exe
          else
            for i in $PREFIX/bin/*.opt.exe ; do
              rm ${i%.opt.exe}.exe
              mv $i ${i%.opt.exe}.exe
            done
          fi
          cd ../findlib-1.7.3
          quietly_log "./configure && make all opt && make install"
          cd ../dune-1.0.1
          quietly_log "ocaml bootstrap.ml && ./boot.exe && cp _build/default/bin/main.exe $PREFIX/bin/dune.exe"
          cd ../easy-format-1.2.0
          quietly_log "make && make install"
          cd ../biniou-1.2.0
          quietly_log "make && ocamlfind install biniou _build/install/default/lib/biniou/*"
          if [[ $OCAML_BRANCH -ge 403 ]] ; then
            cd ../ocamlbuild-0.11.0
            quietly_log "make -f configure.make all OCAMLBUILD_PREFIX=$PREFIX OCAMLBUILD_BINDIR=$PREFIX/bin OCAMLBUILD_LIBDIR=$(ocamlfind printconf path) OCAML_NATIVE=true OCAML_NATIVE_TOOLS=false && make all findlib-install"
            rm $PREFIX/bin/ocamlbuild.{byte,native}.exe
          fi
          cd ../cppo-1.5.0
          quietly_log "make PREFIX=$PREFIX opt install-bin"
          cd ../yojson-1.6.0
          quietly_log "make && ocamlfind install yojson _build/install/default/lib/yojson/*"
          # Remove unnecessary commands to keep the build cache size down
          rm $PREFIX/bin/{ocaml,ocamlcp,ocamldebug,ocamldoc,ocamlmktop,ocamlobjinfo,ocamloptp,ocamlprof}.exe $PREFIX/lib/{expunge,extract_crc,objinfo_helper}.exe
          # Remove unnecessary files
          if [[ $OCAML_BRANCH -lt 405 && $OCAML_BRANCH -gt 402 ]] ; then
            rm $PREFIX/*.txt
          fi
          find $PREFIX -name *.cmt* | xargs rm
          find $PREFIX -name *.ml* | xargs rm
          rm -f $PREFIX/lib/compiler-libs/*.cmx* $PREFIX/lib/compiler-libs/*.{lib,a} $PREFIX/lib/compiler-libs/ocamloptcomp.cma
          echo "Complete"
          appveyor PushArtifact $(echo $LOG_FILE| cygpath -m -f -)
          BUILT_SOMETHING=1
        fi
      fi
    done
    ;;
  matrix)
    for OCAML_VERSION in $OCAML_VERSIONS ; do
      OCAML_BRANCH=${OCAML_VERSION%.*}
      for PORT in mingw mingw64 msvc msvc64 ; do
        if [[ -e $ROOT_CYG/$OCAML_BRANCH/$PORT/bin/ocamlopt.exe ]] ; then
          OUTCOME=None
        else
          OUTCOME=NotRunnable
        fi
        appveyor AddTest "OCaml $OCAML_VERSION ($PORT)" -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Outcome $OUTCOME
      done
    done
    ;;
  build)
    msvs_promote_path $PORT
    ORIG_PATH=$PATH
    for OCAML_VERSION in $OCAML_VERSIONS ; do
      OCAML_BRANCH=${OCAML_VERSION%.*}
      if [[ -e $ROOT_CYG/$OCAML_BRANCH/$PORT/bin/ocamlopt.exe ]] ; then
        echo "Building Merlin $PORT on $OCAML_VERSION"
        SECONDS=0
        appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Running -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration 0
        export PATH=$ROOT_CYG/$OCAML_BRANCH/$PORT/bin:$ORIG_PATH
        mkdir -p $APPVEYOR_BUILD_FOLDER/../merlin-$OCAML_VERSION
        cp -a $APPVEYOR_BUILD_FOLDER $APPVEYOR_BUILD_FOLDER/../merlin-$OCAML_VERSION/$PORT
        cd $APPVEYOR_BUILD_FOLDER/../merlin-$OCAML_VERSION/$PORT
        LOG_FILE=$APPVEYOR_BUILD_FOLDER/build-$OCAML_VERSION-$PORT.log
        rm -f $LOG_FILE
        ERRORS_ALLOWED=1
        if quietly_log "./configure --prefix $ROOT_CYG/$OCAML_BRANCH/$PORT && make" ; then
          appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Running -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration $((SECONDS * 1000))
          if quietly_log "make test" ; then
            # Full pass
            appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Passed -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration $((SECONDS * 1000))
          elif ((!TESTSUITE_SOUND)) ; then
            # Permitted fail
            appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Passed -StdOut "$(tail -10 $LOG_FILE)" -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration $((SECONDS * 1000))
          else
            # Failure
            appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Failed -StdOut "$(tail -10 $LOG_FILE)" -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration $((SECONDS * 1000))
          fi
        else
          # Build failure
          appveyor UpdateTest "OCaml $OCAML_VERSION ($PORT)" -Outcome Failed -StdOut "$(tail -10 $LOG_FILE)" -Framework "OCaml $OCAML_VERSION" -Filename "ocamlmerlin-test.exe" -Duration $((SECONDS * 1000))
        fi
        appveyor PushArtifact $(echo $LOG_FILE| cygpath -m -f -)
      else
        echo "OCaml $OCAML_VERSION for $PORT does not appear to have been built -- skipping"
      fi
    done
    ;;
esac
