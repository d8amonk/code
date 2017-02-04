from swampy.TurtleWorld import *
import math

world = TurtleWorld()
bob = Turtle()
bob.delay = 0.05

# for i in range(4):
#    fd(bob, 100)
#    lt(bob)
# ^^ encapsulate with...
def square(t, length):
    for i in range(4):
        fd(t, length)
        lt(t)

# generalization square as polyline by
# parameterizing angle higher in script
def polyline(t, n, length, angle):
    """Draws n line segments with the given length and
    angle (in degrees) between them. t is a turtle.
    """
    for i in range(n):
        ft(t, length)
        lt(t, angle)

def polygon(t, n, length):
    angle = 360.0/n
    # for i in range(n):
    #     fd(t, length)
    #     lt(t, angle)
    # ^^generalizes with
    polyline(t, n, length)

def circle(t, r):
    circumfrence = 2*math.pi*r
    n = 50
    length = circumfrence/n
    polygon(t, n, length)

# one of the limitations of this is that n is constant
# need to choose a more flexible n without cluttering up
# the interface (generalize n)

def circle(t, r):
    circumfrence = 2*math.pi*r
    n = int(circumfrence/3)+1
    # each length is now 3
    length = circumfrence/n
    polyline(t, n, length, angle)

def arc(t, r, angle):
    arc_length = 2*math.pi*r/angle
    n = int(arc_length/3)+1
    step_length = arc_length/n
    step_angle = float(angle)/n
    # before polyline generalization
    # for i in range(n):
    #     fd(t, step_length)
    #     lt(t, step_angle)
    # ^^factored out and became a call to
    polyline(t, n, step_length, step_angle)

# the definition of circle above thus refactors to
def circle(t, r):
    arc(t, r, 360)





