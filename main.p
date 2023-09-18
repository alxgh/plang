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
x = 14;
if (x == 12) {
    print "HI!";
} else if (x == 13) {
    print "DOOM!";
} else {
    print "OOPS!";
}