// Comments, comments, comments
fn main() // This is a single-line comment
/* This is a multi-line comment */
{
    // This is a single-line comment
    let x = 3; // This is another single-line comment

    /*
     * This is a multi-line comment
     * x = 4; // This will not run
     */

    let x = 5; // This shadows the previous x

    /*
        x = 7;
        This is a nested multi-line comment (level 0)
        /*
            x = 8;
            This is a nested multi-line comment (level 1)
            /*
                x = 9;
                This is a nested multi-line comment (level 2)
                x = 10;
            */
            Back to level 1. Here is another comment: /* what */
        */
        // Back to level 0.
        Here are some stars and slashes for good measure:
        *** // * / ** /
    */

    // let x = 11; */ let x = 12;
    // let x = 13; /* let x = 14;

    // x should still be 5
    println!("x = {x}");
}
/* Last comment */
