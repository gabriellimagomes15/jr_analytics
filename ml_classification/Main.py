import pandas as pd
import numpy as np
import csv

if __name__ == '__main__':
	#with open('../data/reviewFinal.csv','rb') as file:
	#	review = csv.read(file)
	#	for r in review:
	#		print(review)
	df = pd.read_csv('data/reviewFinal.csv',encoding = "latin-1")

	print(df.columns)