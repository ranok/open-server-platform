% OSP Server Configuration

% Use FQDNs
{'USEFQDN', false}.

% The name of this node (if the above is true, this must be a FQDN)
{'NODENAME', 'master@localhost'}.

% Port the admin console should listen on
{'ADMINPORT', 9876}.

% The secret cookie
{'COOKIE', 'AMOJDKFJHEJDHJKSJDY'}.

% Auto started applications as {name, port} tuples
{'AUTO_STARTED', [{osp_admin, 9876}]}.

% Allowed diskless client IPs
{'ALLOWED_DISKLESS', ['127.0.0.1']}.

% Whether the application filesystem is just on the master node, or shared (like NFS)
{'SHARED_FS', false}.

% The prefix of the application file area
{'FS_PREFIX', "/tmp"}.

% Where OSP will store the compiled applications
{'APP_DIR', "apps"}.

% Where OSP will store the application source code
{'SRC_DIR', "servlets"}.
