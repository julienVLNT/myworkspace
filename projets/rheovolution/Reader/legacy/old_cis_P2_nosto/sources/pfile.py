# -*- coding: utf-8 -*-


import numpy as np
import os


__author__  = "Julien VALENTIN"
__date__    = "January, 2023"
__email__   = "julien.valentin@umontpellier.fr"
__version__ = "alpha"


current_dir = os.getcwd()                                                                   # ./source directory
work_dir    = "".join ( [directory + "/" for directory in os.getcwd().split("/")[:-1]] )    # project directory
data_dir    = work_dir + "data/Pcompr"                                                      # data directory


with open (data_dir + "/" + "pcompr", "r") as buffer:
    number_of_fields = int (buffer.readline().strip())
    field_names      = [ buffer.readline().strip() for _ in range(number_of_fields) ]
    
    next (buffer)
    
    values           = [ int(val) for val in buffer.readline().strip().split(" ") if val != '' ]
    topology         = { "nele" : values[0],
                         "nver" : values[1],
                         "ngau" : values[2],
                         "ndim" : values[3],
                         "nfac" : values[4],
                         "npbo" : values[5] }    # number of ... [elements, vertices, gauss points, dimensions, facets, pressure points]
    
    next (buffer)

    elements = np.zeros ((topology["nele"], 5), dtype=np.int32)
    for i in range (topology["nele"]):
        line           = buffer.readline().strip()
        line           = line.split(" ")
        line           = line[1:]
        elements[i, :] = [ int(val) for val in line if val != "" ]

    topology["elem"] = elements

    next (buffer)

    contours = []
    line = buffer.readline().strip()
    while (line != "FACETTES DES MAT. (num_facette num_face)"):
        line = line.split(" ")
        line = [ int(val) for val in line if val != "" ]
        for a in line:
            contours.append (a)

        line = buffer.readline().strip()
    
    contours = np.asarray(contours).flatten()
    topology["cont"] = contours

    facets = []
    for _ in range (topology["nfac"]):
        line = buffer.readline().strip()
        line = line.split(" ")
        line = [ int(val) for val in line if val != "" ]
        line = line[1:]
        facets.append (line)
    facets = np.asarray(facets, dtype=np.int32)
    topology["facets"] = facets

    snapshoots = []
    while (len(buffer.readline().strip()) > 0):
        line   = buffer.readline().strip().split(" ")
        line   = [ float(val) for val in line if val != "" ]
        
        fields = {}
        fields["ktime"] = line[0]
        fields["time"]  = line[1]

        pointwise_fields = []
        for _ in range (topology["nver"]):

            line   = buffer.readline().strip().split(" ")
            line   = [ float(val) for val in line if val != "" ]
            
            pointwise_fields.append (line)
        
        pointwise_fields = np.asarray (pointwise_fields)

        for i, key in enumerate (["coor", "spee", "disp", "temp"]):
            if i < 3:
                fields[key] = pointwise_fields[:, 3*i : 3*i+3]
            else:
                fields[key] = pointwise_fields[:, -1]

        element_fields = []
        for _ in range (topology["nele"]):

            line   = buffer.readline().strip().split(" ")
            line   = [ float(val) for val in line if val != "" ][1:]
            
            element_fields.append (line)
        
        element_fields = np.asarray (element_fields)

        for i, key in enumerate(field_names):
            fields[key] = element_fields[:, i]

        snapshoots.append (fields)
        
        try:
            next (buffer)

        except:
            pass

print (len(snapshoots))
print (fields.keys())
