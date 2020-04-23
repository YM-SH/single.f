import csv
import pprint
import pandas as pd
import matplotlib.pyplot as plt
from bokeh.plotting import figure
from bokeh.io import output_file, show, save, export_png
from bokeh.models import Range1d, PanTool, HoverTool, ColumnDataSource, LabelSet, HoverTool, Legend
from bokeh.layouts import gridplot
import subprocess
import os


print("data.py START")
print(" CALL single.f")
os.chdir("single")
single1 = "gfortran single.f"
subprocess.call(single1.split(), shell=True)
a = "a.exe"
subprocess.call(a.split(), shell=True)
os.chdir("../DATA")


#----------------------------------------------------------------------------------------------------------------------
#Bubble_Radius_before_Injection.csv
#----------------------------------------------------------------------------------------------------------------------
with open('Bubble_Radius_before_Injection.csv', 'r', newline='') as f, open('Bubble_Radius_before_Injection_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('Bubble_Radius_before_Injection_out.csv', header=None, names=['H', 'YP'])
df1 = ColumnDataSource( data = {
    'H' : df["H"],
    'YP' : df["YP"],
})

output_file("../GRAPH/Bubble_Radius_before_Injection.html")
p = figure(title="Bubble_Radius_before_Injection", plot_width=1000, plot_height=900, toolbar_location="above")
p.xaxis.axis_label = "H * FLOAT(I)"
p.yaxis.axis_label = "YP"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("H * FLOAT(I)", "@H"), ("YP", "@YP")])
p.add_tools(hover)
p.line('H', 'YP', legend_label="Bubble_Radius_before_Injection", source = df1)
save(p)


#----------------------------------------------------------------------------------------------------------------------
#DMV1-DMV2-DMV3.csv
#----------------------------------------------------------------------------------------------------------------------
with open('DMV1-DMV2-DMV3.csv', 'r', newline='') as f, open('DMV1-DMV2-DMV3_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('DMV1-DMV2-DMV3_out.csv', header=None, names=['TIME','DMV1','DMV2','DMV3','None'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'DMV1' : df['DMV1'],
    'DMV2' : df['DMV2'],
    'DMV3' : df['DMV3']
})

output_file("../GRAPH/DMV1-DMV2-DMV3.html")
p3 = figure(title="TIME-DMV", plot_width=600, plot_height=900, toolbar_location="above")
p = figure(title="TIME-DMV1", plot_width=600, plot_height=900, toolbar_location="above", x_range=p3.x_range, y_range=p3.y_range)
p1 = figure(title="TIME-DMV2", plot_width=600, plot_height=900, toolbar_location="above", x_range=p3.x_range, y_range=p3.y_range)
p2 = figure(title="TIME-DMV3", plot_width=600, plot_height=900, toolbar_location="above", x_range=p3.x_range, y_range=p3.y_range)
p.xaxis.axis_label = "TIME"
p1.xaxis.axis_label = "TIME"
p2.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "DMV1"
p1.yaxis.axis_label = "DMV2"
p2.yaxis.axis_label = "DMV3"
p3.xaxis.axis_label = "TIME"
p3.yaxis.axis_label = "DMV"
p.toolbar.logo = None
p1.toolbar.logo = None
p2.toolbar.logo = None
p3.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("DMV1", "@DMV1`")])
hover1 = HoverTool(tooltips=[("TIME", "@TIME"), ("DMV2", "@DMV2")])
hover2 = HoverTool(tooltips=[("TIME", "@TIME"), ("DMV3", "@DMV3")])
p.add_tools(hover)
p1.add_tools(hover1)
p2.add_tools(hover2)
p.line('TIME', 'DMV1', legend_label="TIME-DMV1", source = df1, color = "red")
p1.line('TIME', 'DMV2', legend_label="TIME-DMV2", source = df1, color = "blue")
p2.line('TIME', 'DMV3', legend_label="TIME-DMV3", source = df1, color = "gold")
p3.line('TIME', 'DMV1', legend_label="TIME-DMV1", source = df1, color = "red")
p3.line('TIME', 'DMV2', legend_label="TIME-DMV2", source = df1, color = "blue")
p3.line('TIME', 'DMV3', legend_label="TIME-DMV3", source = df1, color = "gold")
p3.legend.click_policy = "hide"
all = gridplot([[p3],[p, p1, p2]])
save(all)


#----------------------------------------------------------------------------------------------------------------------
#MV082.9.csv
#----------------------------------------------------------------------------------------------------------------------
with open('MV082.9.csv', 'r', newline='') as f, open('MV082.9_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('MV082.9_out.csv', header=None, names=['TIME', 'MV082'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'MV082' : df['MV082'],
})

output_file("../GRAPH/MV082.9.html")
p = figure(title="MV082.9", plot_width=1000, plot_height=900, toolbar_location="above")
p.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "MV"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("MV", "@MV082")])
p.add_tools(hover)
p.line('TIME', 'MV082', legend_label="MV082.9", source = df1)
save(p)


#----------------------------------------------------------------------------------------------------------------------
#MOL082.9.csv
#----------------------------------------------------------------------------------------------------------------------
with open('MOL082.9.csv', 'r', newline='') as f, open('MOL082.9_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('MOL082.9_out.csv', header=None, names=['TIME', 'MOL'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'MOL' : df['MOL'],
})

output_file("../GRAPH/MOL082.9.html")
p = figure(title="MOL082.9", plot_width=1000, plot_height=900, toolbar_location="above")
p.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "MOL"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("MOL", "@MOL")])
p.add_tools(hover)
p.line('TIME', 'MOL', legend_label="MOL082.9", source = df1)
save(p)


#----------------------------------------------------------------------------------------------------------------------
#RDR_RSP.csv
#----------------------------------------------------------------------------------------------------------------------
with open('RDR_RSP.csv', 'r', newline='') as f, open('RDR_RSP_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('RDR_RSP_out.csv', header=None, names=['TIME','RDR','RSP','None'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'RDR' : df['RDR'],
    'RSP' : df['RSP']
})

output_file("../GRAPH/RDR_RSP.html")
p2 = figure(title="TIME-DIAMETER", plot_width=1000, plot_height=900, toolbar_location="above")
p = figure(title="TIME-RDR", plot_width=1000, plot_height=900, toolbar_location="above", x_range=p2.x_range)
p1 = figure(title="TIME-RSP", plot_width=1000, plot_height=900, toolbar_location="above", x_range=p2.x_range)
p.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "RDR"
p1.xaxis.axis_label = "TIME"
p1.yaxis.axis_label = "RSP"
p2.xaxis.axis_label = "TIME"
p2.yaxis.axis_label = "DIAMETER"
p.toolbar.logo = None
p1.toolbar.logo = None
p2.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("RDR", "@RDR")])
hover1 = HoverTool(tooltips=[("TIME", "@TIME"), ("RSP", "@RSP")])
p.add_tools(hover)
p1.add_tools(hover1)
p2.add_tools(hover)
p2.add_tools(hover1)
p.line('TIME', 'RDR', legend_label="TIME-RDR", source = df1, color="red", line_width=5)
p1.line('TIME', 'RSP', legend_label="TIME-RSP", source = df1, color="gold", line_width=5)
p2.line('TIME', 'RDR', legend_label="TIME-RDR", source = df1, color="red", line_width=5)
p2.line('TIME', 'RSP', legend_label="TIME-RSP", source = df1, color = "gold", line_width=5)
all = gridplot([[p2],[p, p1]])
save(all)


#----------------------------------------------------------------------------------------------------------------------
#MV3.csv
#----------------------------------------------------------------------------------------------------------------------
with open('MV3.csv', 'r', newline='') as f, open('MV3_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('MV3_out.csv', header=None, names=['TIME', 'MV3'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'MV3' : df['MV3'],
})

output_file("../GRAPH/MV3.html")
p = figure(title="MV3", plot_width=1000, plot_height=900, toolbar_location="above")
p.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "MV3"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("MV3", "@MV3")])
p.add_tools(hover)
p.line('TIME', 'MV3', legend_label="MV3", source = df1)
save(p)


#----------------------------------------------------------------------------------------------------------------------
#T_inside_nozzle.csv
#----------------------------------------------------------------------------------------------------------------------
with open('T_inside_nozzle.csv', 'r', newline='') as f, open('T_inside_nozzle_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace=True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('T_inside_nozzle_out.csv', header=None, names = ['TIME', 'T_inside_nozzle', 'None'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'T_inside_nozzle' : df['T_inside_nozzle'],
})

output_file("../GRAPH/TIME-T.html")
p = figure(title="T_inside_nozzle", plot_width=1000, plot_height=900, toolbar_location="above")
p.xaxis.axis_label = "TIME"
p.yaxis.axis_label = "T_inside_nozzle"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("TIME", "@TIME"), ("T_inside_nozzle", "@T_inside_nozzle")])
p.add_tools(hover)
p.line('TIME', 'T_inside_nozzle', legend_label="TIME-T_inside_nozzle", source = df1, color="red")

p2 = figure(title="TIME-T", plot_width=1000, plot_height=900, toolbar_location="above")
p2.xaxis.axis_label = "TIME"
p2.yaxis.axis_label = "T"
p2.toolbar.logo = None
p2.add_tools(hover)
p2.line('TIME', 'T_inside_nozzle', legend_label="TIME-T_inside_nozzle", source = df1, color="red", line_width=3)


#----------------------------------------------------------------------------------------------------------------------
#T_outside_nozzle.csv
#----------------------------------------------------------------------------------------------------------------------
with open('T_outside_nozzle.csv', 'r', newline = '') as f, open('T_outside_nozzle_out.csv', 'w', newline='') as fo:
    reader = csv.reader(f, delimiter=' ', skipinitialspace = True)
    writer = csv.writer(fo)
    writer.writerows(reader)

df = pd.read_csv('T_outside_nozzle_out.csv', header=None, names=['TIME', 'T_outside_nozzle', 'None'])
df1 = ColumnDataSource( data = {
    'TIME' : df['TIME'],
    'T_outside_nozzle' : df['T_outside_nozzle'],
})

p1 = figure(title="T_outside_nozzle", plot_width=1000, plot_height=900, toolbar_location="above")
p1.xaxis.axis_label = "TIME"
p1.yaxis.axis_label = "T_outside_nozzle"
p1.toolbar.logo = None
hover1 = HoverTool(tooltips=[("TIME", "@TIME"), ("T_outside_nozzle", "@T_outside_nozzle")])
p1.add_tools(hover1)
p1.line('TIME', 'T_outside_nozzle', legend_label="TIME-T_outside_nozzle", source = df1, color="blue")

p2.add_tools(hover1)
p2.line('TIME', 'T_outside_nozzle', legend_label = 'TIME-T_outside_nozzle', source = df1, color="blue")

all = gridplot([[p2],[p, p1]])
save(all)

#----------------------------------------------------------------------------------------------------------------------
#VG1.csv
#----------------------------------------------------------------------------------------------------------------------
df = pd.read_csv('VG1.csv', header=None, names=['VG2-VG1'])
n = range(0, len(df))
df1 = ColumnDataSource( data = {
    'ΔVG' : df['VG2-VG1'],
    'N' : n
})

output_file("../GRAPH/VG1.html")
p = figure(title="VG1", plot_width=1000, plot_height=900, toolbar_location="above")
p.yaxis.axis_label = "VG2-VG1"
p.toolbar.logo = None
hover = HoverTool(tooltips=[("N", "@N"), ("VG2-VG1", "@ΔVG")])
p.add_tools(hover)
p.circle('N', 'ΔVG', legend_label="VG2-VG1", source = df1, size=1)
save(p)

print("data.py END")
