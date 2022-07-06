import csv

def find_tax(tbl,brt):
    with open('skatte-tabell.dat') as taxfile:
        for line in taxfile:
            t=line[1-1:5] ; #Ada indexing - 1-5, inclusive
            #print(tbl,"'" + t + "'")
            if t == tbl :
                low=int(line[8-1:12]) ; #Ada indexing - 8-12, inclusive
                hi=int(line[15-1:19])
                tax=int(line[20-1:24])
                #print(low,hi,tax)
                if low <= brt and brt <= hi :
                    return tax
    return 1/0 ;# crash if not found
    
    
print('{car:22s} {brutto:6s} {tax_with_no_car:6s} {tax_with_car:6s} {netto_utan_bil:6s} {netto_med_bil:6s} {cost:6s}'.format(
                car='Bil', brutto='Brutto' ,tax_with_no_car='Skatt utan', tax_with_car='Skatt med',
                netto_utan_bil='Netto utan',netto_med_bil='Netto med',cost='Kostnad' ))

print('{car:22s} {brutto:6s} {tax_with_no_car:6s} {tax_with_car:6s} {netto_utan_bil:6s} {netto_med_bil:6s} {cost:6s}'.format(
                car='--------------------', brutto='------' ,tax_with_no_car='----------', tax_with_car='---------',
                netto_utan_bil='----------',netto_med_bil='---------',cost='-------' ))
    
                
with open('input.csv') as csvfile:
#    reader = csv.DictReader(csvfile)
    reader = csv.DictReader(filter(lambda row: row[0]!='#', csvfile))
    for row in reader:
        car=row['bil']
        table=row['skattetabell']
        brutto=int(row['bruttolön'])
        bla=int(row['BLA'])
        fmv=int(row['FMV'])
        income_after_BLA = brutto - bla
        income_to_find_taxes = income_after_BLA + fmv
        tax_with_no_car=find_tax(table,brutto)
        tax_with_car=find_tax(table,income_to_find_taxes)
        netto_utan_bil = brutto - tax_with_no_car
        netto_med_bil = brutto - bla - tax_with_car
        cost = netto_utan_bil - netto_med_bil

        print('{car:22s} {brutto:6d} {tax_with_no_car:6d} {tax_with_car:10d} {netto_utan_bil:9d} {netto_med_bil:10d} {cost:8d}'.format(
                car=car, brutto=brutto,tax_with_no_car=tax_with_no_car, tax_with_car=tax_with_car,
                netto_utan_bil=netto_utan_bil,netto_med_bil=netto_med_bil,cost=cost ))



