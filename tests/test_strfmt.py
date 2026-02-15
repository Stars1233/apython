# Test % string formatting

# Basic %s
print("hello %s" % "world")         # hello world

# %d
print("x = %d" % 42)                # x = 42

# Multiple args (tuple)
print("%s is %d" % ("answer", 42))   # answer is 42

# %%
print("100%%")                        # 100%

# %r
print("repr: %r" % "abc")           # repr: 'abc'

# Mixed
print("%s=%d (%s)" % ("x", 10, "ok"))  # x=10 (ok)

# Single %s with non-string
print("val: %s" % 123)              # val: 123
print("val: %s" % True)             # val: True
print("val: %s" % None)             # val: None
