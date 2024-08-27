Here are some ideas for exercises you could do to get more comfortable with `cleff`.

# Add an operation

Add a new operation to `Interact`, say to read a number.

What changes do you have to make to get the program to continue to compile?

# Flesh out the chat program

Currently the `chat` program tries to determine whether the user is rich
based on whether they fly first class, but if they don't fly, we don't
really have any way to get this information from them. Instead, try
asking for their salary.

You will probably have to make changes to the tests to support this
new version of the `chat` program. Add some tests for the cases where
the amount is enough to be considered "rich" and where the amount is
not enough to be considered "rich".

# Add a "real" implementation of UserStore

Currently the UserStore effect doesn't actually do anything, so in
real use we lose the information entered by the user. Try to add a
"real" implementation of `UserStore` and hook it up in `main`. Do the
simplest thing that could possibly work, a SQLite database, CSV, using
`show`/`read` against a plaintext file, whatever you want.
