import csv

path = "C:/Users/10114518/Documents/dm/prj-dropout/mixedmodel/datain/"
filename = "SPEET_DEGREE_INFORMATION.csv"
fp=open(path+filename)
reader = csv.reader(fp, delimiter=',')
header=next(reader)
dataset = []
for line in reader:
    d = dict(zip(header, line))
    for field in ['NUMBERECTS', 'NUMBERYEARS']:
        d[field]=int(d[field])
    for field in ['SCOREIMPROVEMENT']:
        if d[field] == 'Si':
            d[field] = True
        else:
            d[field] = False
    dataset.append(d)
print(dataset[6])

