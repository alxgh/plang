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