class add(int):
    def __call__(self,i):
        return add(self+i)