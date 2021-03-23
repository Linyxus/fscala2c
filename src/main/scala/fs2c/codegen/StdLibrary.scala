package fs2c.codegen

class StdLibrary {

}

object StdLibrary {
  trait LibBundle[T] {
    private var loaded: Boolean = false
    
    def isLoaded: Boolean = loaded
    
    def load: T = {
      loaded = true
      bundle
    }
    
    protected def bundle: T
  }
}
