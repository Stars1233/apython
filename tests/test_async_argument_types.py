# What asyncio does with an argument it cannot run.
#
# task_new steps its argument with gen_send, which reads PyGenObject fields
# off it -- so it has to be a coroutine, a generator or an async generator.
# Nothing checked: `gather("hello")` wrapped the string and crashed on the
# first step, several frames from the mistake, and so did every other entry
# point that wraps a coroutine.  A nested gather crashed the same way, and is
# refused here rather than accepted, which is a divergence bugs.md carries.

import asyncio
async def w(n):
    await asyncio.sleep(0)
    return n

print("=== asyncio.run refuses what it cannot run ===")
for bad in ("x", 5, None, [1], object(), b"y"):
    try:
        asyncio.run(bad)
        print("accepted", type(bad).__name__)
    except ValueError:
        print("ValueError", type(bad).__name__)
    except TypeError:
        print("TypeError", type(bad).__name__)

async def main():
    print("=== so do the three that wrap one ===")
    for bad in ("x", None, [1], 2.5):
        for name in ("create_task", "wait_for", "gather"):
            try:
                if name == "create_task":
                    asyncio.create_task(bad)
                elif name == "wait_for":
                    await asyncio.wait_for(bad, 1.0)
                else:
                    await asyncio.gather(bad)
                print(name, "accepted", type(bad).__name__)
            except TypeError:
                print(name, "TypeError", type(bad).__name__)
    print("=== and the real ones still run ===")
    print(await asyncio.gather(w(1), w(2), w(3)))
    t = asyncio.create_task(w(4))
    print(await t)
    print(await asyncio.wait_for(w(5), 5.0))

asyncio.run(main())
print("done")
