epmd should be start first, or the erlang node can not use the global to find the name, and the log is that
Slogan: Kernel pid terminated (application_controller)

add the command to the /etc/rc.local


/usr/local/otp_src_19.2/bin/epmd -daemon
