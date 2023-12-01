import java.nio.file.Path

enum Mode(val filePath: String) {
  case TEST extends Mode("test-puzzle-input")
  case LIVE extends Mode("live-puzzle-input")
}