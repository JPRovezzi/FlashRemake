<?php
// execute the command that creates a file named hello.txt with the content "Hello, world!"
exec("echo Hello, world! > hello.txt");

// return a response to the client
echo "Command executed successfully";
?>
