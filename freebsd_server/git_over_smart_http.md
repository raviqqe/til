# Git Server over Smart HTTP

Smart HTTP is not a protocol but the mechanism for web servers and git
to transfer objects over HTTP as fast as `git` and `ssh` protocols.
It uses `git` command to pack objects and sends them to clients,
which is faster than just using HTTP GET method for each object.

NOTE: We use the `git_daemon` user to run `git http-backend`
in the guide below.


## Installation of tools

```
$ pkg install git nginx fcgiwrap
$ pkg install apache22 # for htpasswd command
```


## Setting up servers

### Setting up a `fcgiwrap` server

Add these lines to `/etc/rc.conf`.

```
fcgiwrap_enable="YES"
fcgiwrap_flags="-c 2"
fcgiwrap_profiles="nginx"
fcgiwrap_nginx_socket="unix:/var/run/fcgiwrap/fcgiwrap.nginx.socket"
fcgiwrap_nginx_user="git_daemon"
```

Run `fcgiwrap`.

```
$ service fcgiwrap start
```

### Setting up a `nginx` server

Edit `/usr/local/etc/nginx/nginx.conf` like the below.

```
server {

  ...

  location / {
    auth_basic "Restricted";
    auth_basic_user_file htpasswd; # /usr/local/etc/nginx/htpasswd in my case

    include fastcgi_params;
    fastcgi_param SCRIPT_FILENAME /usr/local/libexec/git-core/git-http-backend;
    fastcgi_param GIT_PROJECT_ROOT /home/git;
    fastcgi_param GIT_HTTP_EXPORT_ALL "";
    fastcgi_param PATH_INFO $uri; # not $fastcgi_path_info!
    fastcgi_param REMOTE_USER $remote_user; # Is this necessary?
    fastcgi_pass unix:/var/run/fcgiwrap/fcgiwrap.nginx.socket;
  }
}
```

Add these lines to `/etc/rc.conf`.

```
nginx_enable="YES"
```

Run `nginx`.

```
$ service nginx start
```


## Testing

On the server, run:

```
$ cd $git_repo_root
$ sudo htpasswd -cs htpasswd $user_name
$ sudo -u git_daemon git init --bare --shared=group test.git
```

On clients, run:

```
$ git clone http://git.your.domain.com/test.git
$ cd test
$ touch foo.txt
$ git add .
$ git commit -m "first commit"
$ git push
```


## Anonymous read and authenticated write

```
server {

  ...

  location / {
    set $realm "Restricted";

    if ( $arg_service ~ git-upload-pack ) {
      set $realm off;
    }

    if ( $uri ~ /git-upload-pack$ ) {
      set $realm off;
    }

    auth_basic $realm;
    auth_basic_user_file htpasswd;

    ...
  }
}
```


## HTTP 413: request entity too large

Increase the limit of client request body size.

```
http {
  ...

  server {
    client_max_body_size 256M; # set 0 to unlimit it

    ...
  }

  ...
}
```


## Redirecting repository names without `.git` extensions

WIP


## References

- [an article on qiita](http://qiita.com/egnr-in-6matroom/items/2a052339ee0515b31fdf)
- [an article on hatena](http://d.hatena.ne.jp/ono51/touch/20120619/p1)
- [Re: Conditional http auth](https://forum.nginx.org/read.php?2,155385,155391#msg-155391)
- [Module ngx_http_core_module](http://nginx.org/en/docs/http/ngx_http_core_module.html)
