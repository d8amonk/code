def check_fermat(a, b, c, n):
    if (a^n + b^n) == c ^ n:
        print "Holy Smokes, Fermat was Wrong!"
    else:
        print "No, that doesn't work."


def check_fermat():
    for a in range(100):
        for b in range(100):
            for c in range(100):
                for n in range(100):
                    if (a^n + b^n) == c ^ n:
                        print "Holy Smokes, Fermat was Wrong!"
                    else:
                        pass

def draw(t, length, n):
    if n == 0:
        return
    angle = 50
    fd(t, length*n)
    lt(t, angle)
    draw(t, length, n-1)
    rt(t, 2*angle)
    draw(t, length, n-1)
    lt(t, angle)
    bk(t, length*n)