% OSP Server Configuration

% Use FQDNs
-define(USEFQDN, false).

% The name of this node (if the above is true, this must be a FQDN)
-define(NODENAME, 'master').

% Port the admin console should listen on
-define(ADMINPORT, 9876).

% The secret cookie
-define(COOKIE, 'AMOJDKFJHEJDHJKSJDY').

% Auto started applications as {name, port} tuples
-define(AUTO_STARTED, []).

% Allowed diskless client IPs
-define(ALLOWED_DISKLESS, ['127.0.0.1']).
