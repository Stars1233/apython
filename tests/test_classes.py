class Counter:
    def __init__(self, start):
        self.count = start

    def get(self):
        return self.count

    def increment(self):
        self.count = self.count + 1

c = Counter(10)
print(c.get())
c.increment()
c.increment()
c.increment()
print(c.get())
