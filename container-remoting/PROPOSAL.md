＃ Proposal: Attach-style remote development into local container dev environments

> Draft for a `zed-industries/zed` GitHub **Discussion**. Goal: get staff confirmation
> that this is desired before sending PRs (per CONTRIBUTING.md).

## Summary

Zed already supports remote development over three transports that all implement the
`RemoteConnection` trait in `crates/remote/`: **SSH**, **WSL**, and **Docker/Podman
dev containers**. WSL and the dev-container transports do *not* use SSH — they spawn a
local CLI (`wsl.exe`, `docker`/`podman exec`) and speak Zed's RPC protocol over that
child process's stdin/stdout.

This proposal extends that same exec-over-stdio model to **attach-style local container
dev environments**:

- **Apple `container`** (github.com/apple/container) — plain `container exec`, Podman-shaped.
- **Apple `container machine`** — persistent, home-mounted Linux VMs (`container machine run`).
- **Fedora Toolbox** — persistent Podman containers (`toolbox run` / `podman exec`).

These differ from dev containers: dev containers are isolated/ephemeral and built from
`.devcontainer/devcontainer.json`; Toolbox and container machines are **persistent, full
dev environments** that mount your `$HOME` and match your user — meant for normal
day-to-day development, not a controlled build sandbox. SSH into them works but is not
ideal; attaching directly (like WSL) is a much nicer model.

## Motivation

- The transport abstraction already proves exec-over-stdio works without an SSH server.
- Apple's own `container machine` VS Code example only uses SSH because Remote-SSH has no
  other transport — it has to bake an `sshd`/`systemd` image. Zed can attach natively over
  `container machine run` with **zero** in-image setup.
- Container machines and Toolbox automatically mirror `$HOME` and the host user, so the
  repo you edit on the host is the repo you build inside — no copy step.

## Non-goals

- Not replacing SSH remoting.
- Not the dev-container build pipeline (that already exists).
- No emulation of Docker `--network host` on macOS (Apple's model is different — see below).

## Backends

| Backend | CLI | Exec verb | Lifecycle | Home/identity mirrored | Reach a service |
|---|---|---|---|---|---|
| Apple `container` | `container` | `exec -w/-u/-e <id> <cmd>` | manual `container start` | no | vmnet IP / `<name>.test` DNS |
| Apple `container machine` | `container` | `machine run -n <name> -w/-e/-u -- <cmd>` | **auto-boots** | **yes** | vmnet IP / DNS |
| Fedora Toolbox | `toolbox` / `podman` | `run -c <name> -- <cmd>` | auto-starts | yes | host network (`localhost`) |

### Networking (important nuance)

Apple `container` uses one lightweight VM per container (Containerization on
Virtualization.framework) with a **NAT vmnet network** — each container/machine gets its
own routable IP and a DNS name (default `<name>.test`, or a custom domain via
`sudo container system dns create <domain>`). There is **no** Docker-style `--network host`
shared namespace on macOS.

Consequence: **no port forwarding needed**. You reach an in-machine service at
`my-machine.test:8080` or its vmnet IP — not `localhost`. (The in-machine service must
bind `0.0.0.0`/`::`, not `127.0.0.1`.) Toolbox, by contrast, uses host networking, so its
services are on `localhost` like WSL.

We propose a connect-time **StatusToast** that surfaces the machine's DNS name / IP and a
"Copy address" action, so users know where to reach their services.

## Shared groundwork

Generalize the existing `DockerConnectionOptions.use_podman: bool` into a
`ContainerRuntime` enum (`Docker | Podman | AppleContainer | AppleMachine | Toolbox`) that
encapsulates: binary name, exec verb, lifecycle (manual vs. auto-boot), whether it shares
the host network interface, and binary-deploy strategy. Everything else reuses the
existing `RemoteConnection` trait — no protocol changes.

## Sequenced sub-tasks (each a future PR)

1. **`ContainerRuntime` refactor** — replace `use_podman: bool`; pure refactor, no behavior change.
2. **Apple `container machine` transport** — `container machine run` with `-w/-e/-u`, auto-boot, `$HOME`-prefix discovery. Lowest-risk first backend.
3. **Connect-time address toast** — reusable StatusToast showing DNS/IP + "Copy address".
4. **Apple `container exec`** — falls out of (1) almost for free.
5. **Fedora Toolbox** — `toolbox run -c` (or `podman start` + `podman exec`); host networking.
6. **Pickers / settings / persistence** — enumerate via `container machine ls`, `container ls -a`, `toolbox list`; settings schema + `RemoteConnectionKind` persistence.

## Integration touch points (per backend)

1. Transport in `crates/remote/src/transport/` (+ register in `transport.rs`).
2. `RemoteConnectionOptions` enum + `ConnectionPool::connect` arm + `display_name` + `From` impls in `remote_client.rs`.
3. `RemoteConnectionIdentity` in `remote_identity.rs`.
4. Settings schema in `crates/settings_content/src/settings_content.rs`.
5. Settings wiring in `crates/recent_projects/src/remote_connections.rs`.
6. Persistence: `RemoteConnectionKind` in `crates/workspace/src/persistence/model.rs` + column handling in `persistence.rs`.
7. Picker UI (model on `crates/recent_projects/src/wsl_picker.rs`) + entry in `remote_servers.rs`.

## Open questions for maintainers

- Appetite for a new top-level remote category vs. folding into the existing dev-container UI?
- Does the connect-time address toast fit Zed's notification conventions?
- macOS-26 gating for Apple `container` networking features?
