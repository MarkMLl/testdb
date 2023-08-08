This is experimental code oriented towards a PostgreSQL server, although much of it is applicable to other backends.

It illustrates that DB-aware components are prepared to accept work even if the underlying connection has failed, only throwing an error when a transaction commit is forced. Failure might be caused by, for example, the server daemon timing out due to a laptop running the client having been in a sleep state, or the client IP address being changed.

It includes investigation of the extent to which the user can at least be alerted of potential problems by recovering the client port number from the Postgres state, and using the ident protocol (port 113) to verify that there is a live connection.

This is in no way a complete implementation, for example it should indicate immediately if the client IP address has changed rather that relying on a server timeout.

Any attempt to reconnect is "left as an exercise". However comments from the HA community suggest that loss of a session might result in intractable problems if the state of an in-progress transaction is lost.
