import csv
from rich.table import Table
from rich.console import Console
#from rich.table import Column


t=Table(title='Leasingbil verklig månadskostnad')
c=Console()

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
    
    
    
t.add_column('Bil')
t.add_column('Bruttolön', justify='right')
t.add_column('Skatt utan bil', justify='right')
t.add_column('Skatt med bil', justify='right')
t.add_column('Netto utan bil', justify='right')
t.add_column('Netto med bil', justify='right')
t.add_column('Kostnad', justify='right')


    
#print('{car:22s} {brutto:6s} {tax_with_no_car:12s} {tax_with_car:6s} {netto_no_car:6s} {netto_with_car:6s} #{cost:6s}'.format(
#                car='Bil', brutto='Brutto' ,tax_with_no_car='Skatt utan bil', tax_with_car='Skatt med bil',
#                netto_no_car='Netto utan bil',netto_with_car='Netto med bil',cost='Kostnad' ))
#
#print('{car:22s} {brutto:6s} {tax_with_no_car:12s} {tax_with_car:6s} {netto_no_car:6s} {netto_with_car:6s} #{cost:6s}'.format(
#                car='-----------------------',
#                brutto='------' ,
#                tax_with_no_car='-------------',
#                tax_with_car='---------',
#                netto_no_car='----------',
#                netto_with_car='---------',
#                cost='-------' ))
    
                
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
        netto_no_car = brutto - tax_with_no_car
        netto_with_car = brutto - bla - tax_with_car
        cost = netto_no_car - netto_with_car
        t.add_row(car, str(brutto), str(tax_with_no_car), str(tax_with_car),
                str(netto_no_car), str(netto_with_car), str(cost))

#        print('{car:22s} {brutto:6d} {tax_with_no_car:12d} {tax_with_car:12d} {netto_no_car:9d} #{netto_with_car:10d} {cost:8d}'.format(
#                car=car, brutto=brutto,tax_with_no_car=tax_with_no_car, tax_with_car=tax_with_car,
#                netto_no_car=netto_no_car,netto_with_car=netto_with_car,cost=cost ))

c.print(t)


