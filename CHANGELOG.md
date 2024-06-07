# Unreleased

### Added
- Process::info to get debug information on a specific local process.
- `console` feature which enables a remote debug api for gathering debug information in realtime.

### Changed

### Fixed

# 0.1.25

### Fixed
- Fixed crash when encoding multiple remote protocol frames at once.

# 0.1.24

### Fixed
- Fixed remote protocol performance on windows platforms.

# 0.1.23

### Changed
- Heavily optimized `HashRing` for fast reads.
- Added unit tests for `HashRing`.

# 0.1.22

### Changed
- Improved remote network protocol performance.

# 0.1.21

### Changed
- Added a nicer api for changing the application config.

# 0.1.20

### Added
- Method to cancel timers returned from `send_after`/`cast_after`.
- Method to read timers returned from `send_after`/`cast_after`.

# 0.1.19

### Changed
- Added `RegistryOptions` for Registry::start/start_link/child_spec.
- Added `SupervisorOptions` for Supervisor::start/start_link/child_spec.
- Ability to customize the process name of the registry (use at own risk).

### Fixed
- Fixed warnings when tracing feature is disabled.

# 0.1.18

### Added
- Added a hash ring implementation.

### Changed
- Updated documentation for `OwnedSemaphore`.

# 0.1.17

### Fixed
- Fixed issue initializing websocket handlers.

# 0.1.16

### Changed
- Removed json feature, hydra provides the best case codec for the internal transport.

# 0.1.15

### Fixed
- Fixed incorrect logging of the supervisor pid when applications shutdown.

# 0.1.13

### Added
- Expose configuration values for maximum inbound message sizes from clients.

# 0.1.12

### Fixed
- Actually fix macro build issue...

# 0.1.11

### Fixed
- Fixed issue building without the macros feature in hydra.

# 0.1.10

### Fixed
- Fixed default features being forced when using websockets crate.

# 0.1.9

### Changed
- Reworked semaphore api so that we can create an owned semaphore that works in dynamic environments (not static).

# 0.1.8

### Changed
- Websocket commands api now has a buffer instead of returning a vec.

### Fixed
- Websocket command execution respects a close command.

# 0.1.7

### Added
- Support for tls websockets using `native-tls` feature.

# 0.1.6

### Fixed
- Use a custom fork of dashmap 'hydra-dashmap' to fix the issues during panics until dashmap is fixed.