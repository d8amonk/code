#!/usr/bin/python

# run it like this:
# python AutomatingBidAmounts.py cpa roku_test_raw.csv outfile

import sys
import pandas as pd
import numpy as np
import scipy.stats as stats


# get the args from the cmd prompt
args = sys.argv
cpa = sys.argv[1]
csv_input = sys.argv[2]
csv_output = sys.argv[3]

df = pd.read_csv(csv_input)

# print(cpa)
# print(csv_input)

def auto_bid_sys(cpa, df, out):

	# rename cols b/c "." in col name breaks stuff
	df.rename(columns={'Search keyword':'Keyword'}, inplace = True)
	df.rename(columns={'Ad group':'ad_group'}, inplace = True)

	# has $
	df.rename(columns={'Avg. CPC':'avg_cpc'}, inplace = True)
	df['avg_cpc'] = df['avg_cpc'].map(lambda x: x.lstrip('$'))
	pd.to_numeric(df.avg_cpc)

	df.rename(columns={'Avg. position':'avg_position'}, inplace = True)
	df.rename(columns={'Converted clicks':'converted_clicks'}, inplace = True)

	# has $
	df.rename(columns={'Cost / converted click':'cost_converted_clicks'}, inplace = True)
	df['cost_converted_clicks'] = df['cost_converted_clicks'].map(lambda x: x.lstrip('$'))
	pd.to_numeric(df.cost_converted_clicks)

	# has %
	df.rename(columns={'Click conversion rate':'click_conversion_rate'}, inplace = True)
	df['click_conversion_rate'] = df['click_conversion_rate'].map(lambda x: x.rstrip('%'))
	pd.to_numeric(df.click_conversion_rate)

	df.rename(columns={'View-through conv.':'view_through_conv'}, inplace = True)

	# has $
	df.rename(columns={'Cost / conv.':'cost_conv'}, inplace = True)
	df['cost_conv'] = df['cost_conv'].map(lambda x: x.lstrip('$'))
	pd.to_numeric(df.cost_conv)

	# has %
	df.rename(columns={'Conv. rate':'conv_rate'}, inplace = True)
	df['conv_rate'] = df['conv_rate'].map(lambda x: x.rstrip('%'))
	pd.to_numeric(df.conv_rate)

	df[['avg_cpc','cost_converted_clicks', 'click_conversion_rate', 'cost_conv', 'conv_rate']] = df[['avg_cpc','cost_converted_clicks', 'click_conversion_rate', 'cost_conv', 'conv_rate']].apply(pd.to_numeric)

	# determine type
	df['Type'] = np.where(df.Keyword.str.contains('\['), 'Exact', np.where(df.Keyword.str.contains('\+'), 'Broad', 'Phrase'))

	# create standalone df per type (so they can be compared to the whole individually)
	df_exact = df[(df.Type=="Exact")]
	df_broad = df[(df.Type=="Broad")]
	df_phrase = df[(df.Type=="Phrase")]

	#Exact
	if(len(df_exact) > 0):

		df_exact_conv_wt = sum(df_exact.Conversions)/sum(df.Conversions)

		# calculate percent rank
		x = np.array(df_exact.Conversions)
		df_exact['Percentile'] = [stats.percentileofscore(x, a, 'rank') for a in x]
		df_exact['Conv_weight'] = df_exact.Conversions/sum(df_exact.Conversions)
		df_exact['Initial_bid'] = np.where((df_exact.conv_rate > 1) | (df_exact.Clicks < 100), round(df_exact.conv_rate*df_exact.Conv_weight*float(cpa),2), round(df_exact.conv_rate*float(cpa),2))
		df_exact['Type_adj'] = 1+df_exact_conv_wt
		df_exact['Percent_adj'] = 1+df_exact.Percentile
		df_exact['Final_bid'] = df_exact.Initial_bid*df_exact.Type_adj*df_exact.Percent_adj


	#Broad
	if(len(df_broad) > 0):

		df_broad_conv_wt = sum(df_broad.Conversions)/sum(df.Conversions)

		# calculate percent rank
		x = np.array(df_broad.Conversions)
		df_broad['Percentile'] = [stats.percentileofscore(x, a, 'rank') for a in x]
		df_broad['Conv_weight'] = df_broad.Conversions/sum(df_broad.Conversions)
		df_broad['Initial_bid'] = np.where((df_broad.conv_rate > 1) | (df_broad.Clicks < 100), round(df_broad.conv_rate*df_broad.Conv_weight*float(cpa),2), round(df_broad.conv_rate*float(cpa),2))
		df_broad['Type_adj'] = 1+df_broad_conv_wt
		df_broad['Percent_adj'] = 1+df_broad.Percentile
		df_broad['Final_bid'] = df_broad.Initial_bid*df_broad.Type_adj*df_broad.Percent_adj


	#Phrase
	if(len(df_phrase) > 0):

		df_phrase_conv_wt = sum(df_phrase.Conversions)/sum(df.Conversions)

		# calculate percent rank
		x = np.array(df_phrase.Conversions)
		df_phrase['Percentile'] = [stats.percentileofscore(x, a, 'rank') for a in x]
		df_phrase['Conv_weight'] = df_phrase.Conversions/sum(df_phrase.Conversions)
		df_phrase['Initial_bid'] = np.where((df_phrase.conv_rate > 1) | (df_phrase.Clicks < 100), round(df_phrase.conv_rate*df_phrase.Conv_weight*float(cpa),2), round(df_phrase.conv_rate*float(cpa),2))
		df_phrase['Type_adj'] = 1+df_phrase_conv_wt
		df_phrase['Percent_adj'] = 1+df_phrase.Percentile
		df_phrase['Final_bid'] = df_phrase.Initial_bid*df_phrase.Type_adj*df_phrase.Percent_adj


	df_final = pd.concat([df_exact,df_broad,df_phrase])

	keyword_final = df_final['Keyword'].tolist()
	type_final = df_final['Type'].tolist()
	bid_final = df_final['Final_bid'].tolist()

	keyword_series = pd.Series(keyword_final)
	type_series = pd.Series(type_final)
	bid_series = pd.Series(bid_final)

	df_final = pd.DataFrame({'Keyword':keyword_series, 'Type':type_series, 'Final_bid':bid_final})

	df_final.to_csv(csv_input)

if __name__ == '__main__':
    print(auto_bid_sys(cpa, df, csv_output))

