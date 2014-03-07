class Fls():
    def __init__(self):
        pass

    def vrednost(self, v):
        return False

class Tru():
    def __init__(self):
        pass

    def vrednost(self, v):
        return True

class Var():
    def __init__(self, ime):
        self.ime = ime

    def vrednost(self, v):
        return v[self.ime]


class Not():
    def __init__(self, p):
        self.formula = p

    def vrednost(self, v):
        return not (self.formula.vrednost(v))

class And():
    def __init__(self, ps):
        self.formule = ps

    def vrednost(self, v):
        b = True
        for p in self.formule:
            b = b and p.vrednost(v)
            if not b: break
        return b

class Or():
    def __init__(self, ps):
        self.formule = ps

    def vrednost(self, v):
        b = False
        for p in self.formule:
            b = b or p.vrednost(v)
            if b: break
        return b

# Primer
moja_formula = And([Var("p"), Not(Var("p"))])

# Valuacija: prireditev vrednosti True/False spremenljivkam

v = { "p" : True, "q" : False }

print (moja_formula.vrednost(v))
