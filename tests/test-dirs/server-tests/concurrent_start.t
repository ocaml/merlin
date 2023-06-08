In case server is running, stop it.

  $ $MERLIN server stop-server

Check that the server is able to start when running commands concurrently:

  $ $MERLIN server errors << EOF & $MERLIN server errors << EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN server stop-server

