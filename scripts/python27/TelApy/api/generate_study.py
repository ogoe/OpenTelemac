#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    @package generate_study
    Tools to generate python, yacs scripts from data

    Author(s): Fabrice Zaoui, Yoann Audouin, Cedric Goeury

    Copyright EDF 2016
"""
from os import linesep
import types

# Template for the Python script of a study
study_template =\
"""#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Class {module} import
from TelApy.api.t2d import {module}
from mpi4py import MPI
import os

def study_function({inputs}):
    comm = MPI.COMM_WORLD
    # Moving to the study folder (folder containing the steering file)
    os.chdir(os.path.dirname('{steering_file}'))
    # Creation of the instance {module}
    my_module = {module}('{steering_file}', user_fortran='{user_fortran}', comm=comm)
    {read_case_getset}
    # Reading the steering file informations
    my_module.set_case()
    {init_getset}
    # Initalization
    my_module.init_state_default()
    {run_getset}
    # Run all time steps
    my_module.run_all_time_steps()
    {finalize_getset}
    # Ending the run
    my_module.finalize()
    {del_getset}
    # Instance delete
    del(my_module)

    return {outputs}
"""

POS_READ_CASE = 0
POS_INIT = 1
POS_RUN = 2
POS_FINALIZE = 3
POS_DEL = 4

VARINFO = {}

def jdc_to_dict(jdc, command_list):
    """
    This tricky function transforms a JdC with a single command into a
    dictionary that can be used more easily from a Python context (thanks to
    M. Courtois and G. Boulant).
    """
    context = {}
    for command in command_list:
        context[command] = _args_to_dict
    exec "parameters = " + jdc.strip() in context
    return context['parameters']

def _args_to_dict(**kwargs):
    return kwargs

def get_jdc_dict_var_as_tuple(jdc_dict, varname):
    if not jdc_dict.has_key(varname):
        return tuple()
    elif not isinstance(jdc_dict[varname], types.TupleType):
        return (jdc_dict[varname],)
    else:
        return jdc_dict[varname]

def generate_set(input_variable):
    """
    Will generate a string containing a call to the set of the api

    @param input_variable Eficas input_variable rubrique

    @returns a string
    """

    string = ""

    # Getting name of python variable
    name = input_variable['NAME']
    # Getting name of the variable in the API
    var_name = input_variable['VAR_INFO']['VAR_NAME']

    # Checking what type of zone definition we have
    if 'INDEX' in input_variable['VAR_INFO']['ZONE_DEF']:
        i, j, k = input_variable['VAR_INFO']['ZONE_DEF']['INDEX']
        string = "my_study.set('%s', %s, i=%i; j=%i, k=%i)"%(var_name, name, i, j, k)
    elif 'RANGE' in input_variable:
        # TODO: See how to handle ranges
        string = "my_study.set_on_range('%s', '%s')"%(name, var_name, i, j, k)
    elif 'POLYGON' in input_variable:
        poly = input_variable['VAR_INFO']['ZONE_DEF']['POLYGON']
        # TODO: Check type of poly
        string = "my_study.set_on_polygon('%s', %s, %s)"%(var_name, name, str(poly))
    else:
        raise Exception("Missing Zone definition")

    return string

def generate_get(output_variable):
    """
    Will generate a string containing a call to the get of the api

    @param output_variable Eficas output_variable rubrique

    @returns a string
    """

    string = ""

    # Getting name of python variable
    name = output_variable['NAME']
    # Getting name of the variable in the API
    var_name = output_variable['VAR_INFO']['VAR_NAME']

    # Checking what type of zone definition we have
    if 'INDEX' in output_variable['VAR_INFO']['ZONE_DEF']:
        i, j, k = output_variable['VAR_INFO']['ZONE_DEF']['INDEX']
        string = "%s = my_study.get('%s', i=%i, j=%i, k=%i)"%(name, var_name, i, j, k)
    else:
        raise Exception("Missin Zone definition")

    return string

def handle_variables(jdc):
    """
    Extract all the informations for the input and ouput variables
    """

    input_names = []
    input_types = []
    input_val = []
    output_names = []
    output_types = []
    getset = ["", "", "", "", ""]

    input_vars = get_jdc_dict_var_as_tuple(params, "INPUT_VARIABLE")
    output_vars = get_jdc_dict_var_as_tuple(params, "OUTPUT_VARIABLE")

    input_names = [var["NAME"].strip() for var in input_vars]
    output_names = [var["NAME"].strip() for var in output_vars]

    for var in input_vars:
        varname = var['VAR_INFO']['VAR_NAME'].rstrip()
        pos = VARINFO[varname]['set_pos']
        vartype = VARINFO[varname]['type']
        getset[pos] += generate_set(var) + linesep + ' '*4
        input_val.append(var['VAR_INFO']['DEFAULT_VALUE'])
        input_types.append(vartype)
    for var in output_vars:
        varname = var['VAR_INFO']['VAR_NAME'].rstrip()
        pos = VARINFO[varname]['get_pos']
        vartype = VARINFO[varname]['type']
        output_types.append(vartype)
        getset[pos] += generate_get(var) + linesep + ' '*4

    return input_names, input_types, input_val, output_names, output_types, getset

def build_var_info(jdc):
    """
    Build the VARINFO dictionary

    @param jdc The eficas dictionary
    """
    from TelApy.api.t2d import Telemac2d

    steering_file = jdc['STEERING_FILE']

    t2d = Telemac2d(steering_file)

    VARINFO.update(t2d.generate_var_info())

    del(t2d)

    for key in sorted(VARINFO.keys()):
        print "%s:"%key
        for key2 in VARINFO[key]:
            print "-- %s = %s"%(key2,VARINFO[key][key2])

def generate_study_script(jdc):
    """
    Builds a study function from an Eficas dictionary

    @param jdc The eficas dictionary

    @returns A string containing the python script of the study
    """

    build_var_info(jdc)

    steering_file = jdc['STEERING_FILE']

    user_fortran = jdc.get('USER_FORTRAN', 'None')
    module = 'Telemac2d'

    input_names, _, _, output_names, _, getset = handle_variables(jdc)

    my_study = study_template.format(\
            inputs=", ".join(input_names),\
            outputs=", ".join(output_names),\
            module=module,\
            steering_file=steering_file,\
            user_fortran=user_fortran,
            read_case_getset=getset[0],\
            init_getset=getset[1],\
            run_getset=getset[2],\
            finalize_getset=getset[3],\
            del_getset=getset[4])

    return my_study

def generate_yacs_study(jdc):
    """
    Creates a yacs file from an eficas dictionary

    @param jdc The eficas data

    @returns The string containg the yacs file
    """
    import SALOMERuntime

    # Generating python script for the study
    python_script = generate_study_script(jdc)

    # Computing information on inpout/output variables
    input_names, input_types, input_val, output_names, output_types, _ = handle_variables(jdc)

    SALOMERuntime.RuntimeSALOME.setRuntime()
    r=SALOMERuntime.getSALOMERuntime()
    # Building first Bloc
    p=r.createProc("Function_G")
    p.setProperty("DefaultStudyID", "1")
    # Adding types
    td=p.createType("double", "double")
    ti=p.createType("int", "int")
    ts=p.createType("string", "string")
    tb=p.createType("bool", "bool")
    # Defining container
    # TODO: Are the additional containers useful ?
    cont = p.createContainer("container0", "Salome")
    cont.clearProperties()
    cont1 = p.createContainer("container1", "Salome")
    cont1.clearProperties()
    cont2 = p.createContainer("container2", "Salome")
    cont2.clearProperties()

    # Creating second bloc
    bloc0 = r.createBloc("Function_G")
    p.edAddChild(bloc0)
    # Creating function node
    node0 = r.createFuncNode("","STUDY")
    bloc0.edAddChild(node0)
    node0.setFname("my_study")
    node0.setScript(python_script)
    node0.setExecutionMode("remote")
    node0.setContainer(cont)

    # Adding inputs
    for name, typ, val in zip(input_names, input_types, input_val):
        # Identify yacs type and converting default value
        if typ == "DOUBLE":
            vartyp = td
            varval = float(val)
        elif typ == "STRING":
            vartyp = ts
            varval = val
        elif typ == "INTEGER":
            vartyp = ti
            varval = int(val)
        elif typ == "BOOLEAN":
            vartyp = tb
            varval = bool(val)
        else:
            raise Exception("Unknow type %s for %s"%(typ, name))
        print "name", name
        print "vartype", typ
        print "varval", varval
        var = node0.edAddInputPort(name, vartyp)
        var.edInitPy(varval)

    # Adding outputs
    for name, typ in zip(output_names, output_types):
        # Identify yacs type
        if typ == "DOUBLE":
            vartyp = td
        elif typ == "STRING":
            vartyp = ts
        elif typ == "INTEGER":
            vartyp = ti
        elif typ == "BOOLEAN":
            vartyp = tb
        else:
            raise Exception("Unknow type %s for %s"%(typ, name))
        print "name", name
        print "vartype", typ
        out = node0.edAddOutputPort(name, vartyp)

    return p

if __name__ == "__main__":

    with open('test.comm') as jdcfile:
        jdc = jdcfile.read()
    params = jdc_to_dict(jdc, ['TELEMAC2D', '_F'])

    my_study = generate_study_script(params)

    with open('my_study.py','w') as fobj:
        fobj.write(my_study)

    my_yacs = generate_yacs_study(params)
    my_yacs.saveSchema('my_study.xml')
