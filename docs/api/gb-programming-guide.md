[< back to index](../doc_index.md)

# Game Boy programming guide

**This guide is incomplete.
Support for Game Boy targets is experimental and all information in this document may become obsolete.**

## Program lifecycle

The default Game Boy vectors are defined as following:

The minimal Game Boy program thus looks like this:

    void main() {
        // initialize things
        while(true) { }
    }
    
    void on_vblank() {
        // do things
    }
    
