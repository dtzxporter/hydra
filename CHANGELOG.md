# Unreleased

### Added

### Changed

### Fixed

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