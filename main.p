var x = 13;
print x;
{
    print x;
    x = 12;
    print x;
    var x = 14;
    print x;
}
print x;
x = 15;
if (x == 12 or x == 15) {
    print "HI!";
} else if (x == 13) {
    print "DOOM!";
} else {
    print "OOPS!";
}

var z = 3;

while (z >= 0) {
    print z;
    z = z - 1;
}

for (p, 0..10:+1) {
    print p;
}

for (p, 10..0:-1) {
    print p;
}

for (p, 100..1:/2) {
    print p;
}