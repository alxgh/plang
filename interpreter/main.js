let a = "global";
{
    function showA() {
        console.log(a);
    }

    let a = "block";
    showA.bind(this)();
}