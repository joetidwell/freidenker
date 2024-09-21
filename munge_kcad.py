import json
import csv

###
### Load Data
###

with open('KCAD_2016.txt') as f:
    kcad2016 = json.loads(json.load(f))
with open('KCAD_2024.txt') as f:
    kcad2024 = json.loads(json.load(f))
    
properties = kcad2016 + kcad2024

# Retain only propertyId and appraisedValue
properties = [{'propertyId':x['propertyId'], 
               'year':x['year'],
               'appraisedValue':x['appraisedValue']} for x in properties]
properties = sorted(properties, key=lambda d: d['propertyId'])


###
### Exclude propeerties not present in all years
###

d = dict()
for p in properties:
    d[p['propertyId']] = d.get(p['propertyId'], 0) + 1 
exclude_ids = [k for k in d if d[k] != 2]
properties = [p for p in properties if p['propertyId'] not in exclude_ids]


exclude_ids = set()
for p in properties:
    if not p['appraisedValue']:
        exclude_ids.add(p['propertyId'])
properties = [p for p in properties if p['propertyId'] not in exclude_ids]

###
###
###

keys = properties[0].keys()
with open('KCAD_values.csv', 'w', newline='') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(properties)

