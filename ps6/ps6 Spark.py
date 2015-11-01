'''
ssh jason_wang@radagast.berkeley.edu

export PATH=$PATH:/root/ephemeral-hdfs/bin/

hadoop fs -mkdir /data
hadoop fs -mkdir /data/airline
df -h

mkdir /mnt/airline
cd /mnt/airline

# Download file
wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz

tar -xvzf 1987-2008.csvs.tgz

# 
hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline

# check files on the HDFS
hadoop fs -ls /data/airline

# Install numpy
yum install -y python27-pip python27-devel
pip-2.7 install 'numpy==1.9.2'
/root/spark-ec2/copy-dir /usr/local/lib64/python2.7/site-packages/numpy

export PATH=${PATH}:/root/spark/bin

# start Spark's Python interface as interactive session
pyspark
'''
---------------The following is code for python---------------
from operator import add
import numpy as np
import timeit

lines = sc.textFile('/data/airline')
# Remove NA
def removeNA(line):
	vals = line.split(',')
	return(vals[0] != 'Year' and vals[15] != 'NA')

# Repartition the data
lines = lines.filter(removeNA).repartition(96).cache()

# Count how many line in the data to compare with the result by R
numLines = lines.count()
## 121232833

# Map funcition Flight late
def count_late_flight(line):
	vals = line.split(',')
	CRSDep = int(vals[5]) // 100
	# Key is Uniquecarrier-Origin-Dest-Month-DayOfWeek-CRSDepTime
	keyVals = '-'.join([vals[8],vals[16],vals[17],vals[1],vals[3], str(CRSDep)])
	x1 = 0
	x2 = 0
	x3 = 0
	if int(vals[15]) > 30:
		x1 = 1
	if int(vals[15]) > 60:
		x2 = 1
	if int(vals[15]) > 180:
		x3 = 1
	return(keyVals, [x1, x2, x3, 1])

# Reduce funcition (add up the element in the list)
sum_v = lambda x, y: [x[0] + y[0], x[1] + y[1], x[2] + y[2], x[3] + y[3]]
# Time evaluation
start_time = timeit.default_timer()
Flightlate = lines.map(count_late_flight).reduceByKey(sum_v).collect()
elapsed = timeit.default_timer() - start_time
print "Finish"
Flightlate[0:10]


# Compute the proportion
proportion = lambda x: [round(float(x[0])/float(x[3]), 4), round(float(x[1])/float(x[3]), 4), round(float(x[2])/float(x[3]), 4), x[3]]

# String process, comma-delimited
def strprocess (x):
	return(",".join([x[0], str(x[1]).replace("[", "").replace("]", "")]))

mytry = lines.map(count_late_flight).reduceByKey(sum_v).mapValues(proportion).map(strprocess).repartition(1).saveAsTextFile('/data/FlightLateCount')


# Late flight at least 150
def count_late_flight_150(line):
	vals = line.split(',')
	CRSDep = int(vals[5]) // 100
	# Key is Uniquecarrier-Origin-Dest-Month-DayOfWeek-CRSDepTime
	keyVals = '-'.join([vals[8],vals[16],vals[17],vals[1],vals[3], str(CRSDep)])
	x1 = 0 
	if int(vals[15]) > 0:
		x1 = 1
	return(keyVals, [x1, 1])

def atleast(x):
	return(x[1][1] > 150)

sum_v = lambda x, y: [x[0] + y[0], x[1] + y[1]]

propotion = lambda x: [x[1], round(float(x[0])/float(x[1]), 4)]
atleast150 = lines.map(count_late_flight_150).reduceByKey(sum_v).filter(atleast).mapValues(propotion).sortBy(lambda x: -x[1][1]).collect()

atleast150[0:10]