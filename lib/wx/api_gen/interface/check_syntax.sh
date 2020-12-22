#!/bin/bash

# check_syntax.sh - a small script to search for interface headers with
#                   missing semicolons (they give troubles to Doxygen).
# Author: Francesco Montorsi


rm -f missing_semicolons

# the preprocessor will remove comments and all #preprocessor #stuff;
# we then remove the empty lines
for iface in wx/*h; do

    echo "--- $iface ---" >>missing_semicolons

    gcc -E $iface | grep -v '#' | grep -v '^[[:space:]]*$' >temp

    # now remove the lines which ends with a comma or a semicolon; they're ok
    cat temp | grep -v '.*;$' | grep -v '.*,$' >temp2

    # now search for methods; we know they should always contain at least two () brackets!
    cat temp2 | grep '(' >>missing_semicolons

    # now remove the lines which shouldn't have final comma or semicolon:
#     cat temp2 | grep -v '^[[:space:]]*wx[A-Z]*[[:space:]]*$' >temp
#     cat temp | grep -v 'class' | grep -v 'enum' | grep -v 'template' | \
#                grep -v 'struct' | grep -v 'typedef' | \
#                grep -v 'public:' | grep -v 'protected:' | grep -v 'private:' | \
#                grep -v '{' | grep -v '}' >>missing_semicolons

done

rm temp temp2
