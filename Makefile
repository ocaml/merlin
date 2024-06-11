all: build ocamlmerlin ocamlmerlin-server dot-merlin-reader

build:
	dune build --always-show-command-line

ocamlmerlin ocamlmerlin-server dot-merlin-reader:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test: build
	dune runtest

preprocess:
	dune build --always-show-command-line @preprocess

promote:
	dune promote

bench_disabled:
	merl-an benchmark -p /projects/irmin -s 1 --data=merl-an_bench
	echo "Top 10 slowest queries:"
	jq -s "[ (map(.sample_id |= tostring)) + (map(.sample_id |= tostring)) | group_by(.sample_id)[] | select(length > 1) | add ] | sort_by( .responses[0].timing.clock) | reverse | .[:10]" merl-an_bench/query_responses.json merl-an_bench/commands.json
	echo "Benchmark result:"
	jq . merl-an_bench/bench.json

bench:
	echo ""

.PHONY: all build dev clean test promote bench bench_disabled
