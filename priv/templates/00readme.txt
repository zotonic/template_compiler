This directory contains templates used for testing the template compiler.

Use the template_compiler env setting 'template_dir' to change this to your
own directory.

The 'template_dir' env setting can be one of:

    * a tuple with your app name and directory inside your app's priv dir: {application_name, "path/in/app/priv"}
    * or a direct path to the directory "path/to/the/template/directory"

You can also use your own runtime 'find_template/3' routine for mapping template-names to filenames.
