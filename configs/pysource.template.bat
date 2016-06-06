@rem This file is a template for a Windows environement (Dos) file 
@rem runnning "pysource.template.bat" will position all 
@rem the necessary environement variable for telemac
@rem To adapt to your installation replace word <word> by their local value
@rem
@rem Path to telemac root dir
@set HOMETEL=<path_to_telemac_root_dir>
@rem Adding python scripts to PATH
@set PATH=%HOMETEL%\scripts\python27;%PATH%
@rem Path to this file
@set SOURCEFILE=%HOMETEL%\configs\pysource-win.bat
@rem Configuration file
@set SYSTELCFG=%HOMETEL%\configs\systel.cfg
@rem Name of the configuration to use
@set USETELCFG=<myconfiguration>
