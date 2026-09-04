# More concurrent tasks than either backend's queue was built for.
#
# The poll backend held 256 timers and 1024 descriptors in fixed arrays, and
# the io_uring backend as many SQEs as the ring has; all three submit paths
# returned as though the wait had been armed when the queue was full, so the
# task waited for a wakeup that could never come and the loop ran forever.
# 300 sleeps in one gather is not an unusual program.
#
# The arrays grow now, and a full submission ring is submitted to the kernel
# and retried -- which is what makes room, since every SQE here is queued
# without an enter() of its own.

import asyncio

N = 400


async def w(i):
    await asyncio.sleep(0.001)
    return i


async def main():
    got = await asyncio.gather(*[w(i) for i in range(N)])
    return len(got), sum(got), got[0], got[-1]


print(asyncio.run(main()))

# The same again, so the second run reuses the grown arrays.
print(asyncio.run(main()))


# Nested: each of these gathers a few of its own.
async def group(base):
    return sum(await asyncio.gather(*[w(base + i) for i in range(10)]))


async def nested():
    return sum(await asyncio.gather(*[group(b * 10) for b in range(30)]))


print(asyncio.run(nested()))


# And a crowd of tasks that finish without ever sleeping.
async def quick(i):
    return i * 2


async def no_sleep():
    return sum(await asyncio.gather(*[quick(i) for i in range(500)]))


print(asyncio.run(no_sleep()))
print("done")
