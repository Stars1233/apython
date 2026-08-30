try:
    from _no_such_accelerator import speedup
    chosen = "c"
except ImportError:
    chosen = "python"
