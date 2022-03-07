# AEL: The Aeternity Launcher
The Aeternity launcher is a native, graphical tool to simplify working with Aeternity nodes.

## ALPHA WARNING
AEL is in the experimental Alpha phase and is not yet cross platform.

## How to start AEL
To run AEL you will need:
- A Linux system (it can be made to work on OSX and BSD, but takes more care setting up the environment)
- A full Erlang installation available (R23 or higher with WX enabled)
- Aeternity [build dependencies](https://github.com/aeternity/aeternity/blob/master/docs/build.md) installed for your platform
- [ZX](https://zxq9.com/projects/zomp/qs.install.en.html) installed

If the above requirements are met start AEL with `zx run ael`.

You can also run AEL directly from a clone of this repository (the way we do in development) using `zx runlocal` (from within the top directory of the project) or `zx rundir [path to ael repo]`.

## Rationale
AEL is written in Erlang and runs in the same execution environment as the Aeternity node.
This gives us the ability to implement and experiment with convenience and utility features that are impossible or unreasonably painful to implement another way.

AEL is not a replacement for the web tooling or the features provided by the other SDKs.

AEL started life as a quality of life tool for the Core Team, but has utility for other users so we are releasing it publicly and as features are added they will be packaged and released publicly.

## Roadmap
- Developer tools
  - Local Sophia contract project compilation and deployment
  - GUI access to "dev mode"
  - Arbitrary test wallet functionality
  - Local test network management
  - Manage dev-mode forks from existing chains
- Chain explorer
- Mempool explorer
- Mining configuration and graphical status reporting
- (Bonus points): GUI management of Aeternity Stratum pools and pool membership
