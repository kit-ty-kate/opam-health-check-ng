### How to install opam-health-check-ng:

```
$ opam pin add opam-health-check-ng .
```

### How to use opam-health-check-ng locally:

For opam-health-check-ng to work you need to start the server like so:
```
$ opam-health-serve --debug "$workdir"
```
For instance:
```
$ workdir=/tmp/opam-health-check
$ opam-health-serve --debug "$workdir"
```
`--debug` is optional but recommended to start with.

Now simply use the `opam-health-check` command. First you need to initialize it like so:
```
$ opam-health-check init --from-local-workdir "$workdir"
```

Now you can send any command to the server using the `opam-health-check` command.
All subcommands may be listed with `opam-health-check --help`

### Maximizing performance:

To maximize performance you can set `enable-dune-cache: true` in `config.yaml` as well as
mount `/var/lib/docker` as `tmpfs`. To do so, add the following line to your `/etc/fstab`:
```
tmpfs	/var/lib/docker	tmpfs	nosuid,nodev,size=80%	0	0
```
Keep in mind that `enable-dune-cache: true` will use a significant amount of disk space (hundreds of GB)
off of `XDG_CACHE_HOME` (by default: `$HOME/.cache`) and mounting `/var/lib/docker` as `tmpfs` requires
a significant amount of RAM (around 25GB for a 32 core system) so it is recommended to have at least
twice the amount of RAM you have of CPU cores activated by `opam-health-check` through the `processes`
configuration option.

### How to use opam-health-check-ng remotely:

As with local opam-health-check you need to have a server started somewhere and accessible.
Don't forget to open the admin and http ports. Default ports are respectively 6666 and 8080.
You can change them by modifying the yaml config file at the root of the work directory and
restarting the server.

During the first run the server creates an admin user and its key.
To connect to the server remotely just you first need to retreive the `admin.key` file located
in `<workdir>/keys/admin.key` and do `opam-health-check init`.
From there, answer all the questions (hostname, admin-port (default: 6666), username (admin)
and the path to the user key you just retreived).
You now have your client tool configured with an admin user!

To add new users, just use the `opam-health-check add-user <username>` command as the admin and
give the key to your new user. She now just need to do the same procedure but with her username.

Side note: every user have the same rights and can add new users.

Enjoy.

## Troubleshooting

**My `config.yaml` file is getting reset when I edit it!**

You should make sure no instance of `opam-health-serve` is running before editing an instance configuration file.

**I started `opam-health-serve`, ran `opam-health-check run`, and nothing seems to be happening.**

Overall this takes a while. You can run `opam-health-check log` to follow the progress. Once it's done, you can visit `http://localhost:port` (where `port` is set in your `config.yaml`) to visualize the results.

**Can I use opam-health-check-ng to build packages on a custom compiler switch?**

Sure, to do that, you need to fork [opam-repository](https://github.com/ocaml/opam-repository/), add your compiler switch to it, and point to your forked repository in the `extra-repositories` field of the `config.yaml` file. E.g.:

```
name: default
port: 8080
# [...]
default-repository: ocaml/opam-repository
extra-repositories:
- alpha:
    github: kit-ty-kate/opam-alpha-repository
    for-switches:
    - 5.0+more_volatile
- more-volatile-switch:
    github: OlivierNicole/opam-repository#more_volatile_switch
    for-switches:
    - 5.0+more_volatile
with-test: false
with-lower-bound: false
list-command: opam list --available --installable --short --all-versions
# [...]
ocaml-switches:
- "4.14": 4.14.0
- "5.0": 5.0.0
- "5.0+more_volatile": ocaml-variants.5.0.0+more_volatile
slack-webhooks: []
```
