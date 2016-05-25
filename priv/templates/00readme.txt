This directory contains templates used for testing the template compile.

Use the template_compiler env setting 'template_dir' to change this to your
own directory.

The env setting can be:

    * {application_name, "path/in/app/priv"}
    * "path/to/the/template/directory"

You can also use your own runtime 'find_template/3' routine for mapping template names to filenames.
