/* 1)  Write a SELECT statement that returns these columns from the INVOICES table 
where the Balance_Due is between 100 and 1000 inclusive

Inv_Number                    The invoice_number column
Inv_Date                          The invoice_date column
Inv_Total                          The invoice_Total column
Payment_plus_Credit      Sum of the payment_total and credit_total
Balance_Due                   invoice_total minus (payment_total plus credit_total) 

Set AP as Default Schema or Use AP; 

NOTE:  The BETWEEN operator is inclusive: begin and end values are included. */

SELECT invoice_number, invoice_date, invoice_total, payment_total + credit_total AS Payment_plus_Credit, 
invoice_total - (payment_total + credit_total) AS Balance_Due
FROM invoices
WHERE invoice_total - (payment_total + credit_total) BETWEEN 100 AND 1000;

/* 2)  Write a SELECT statement that returns these columns from the VENDOR table 
where the vendor_state is "NV" and "NY" but not "NC" or "NJ" - 
use a REGEXP and concat vendor_city and vendor_state with a comma in between 

vendor_id
vendor_city
vendor_state */

SELECT vendor_id, concat(vendor_city, ', ', vendor_state) AS Vendor_city_state
FROM vendors
WHERE vendor_state REGEXP 'N[VY]';

/* 3) Write a SELECT statement that returns these columns from the INVOICE table, 
format the Invoice Date as DD-Mon-YYYY order by invoice_date descending.

Inv_Number                    The invoice_number column
Inv_Date                          The invoice_date column
Inv_Total                          The invoice_Total column */

SELECT invoice_number, DATE_FORMAT(invoice_date, '%e-%b-%Y') AS invoice_date, invoice_total
FROM invoices ORDER BY invoice_date DESC;

/* 4)  Write a SELECT statement that returns these columns from the INVOICES 
table - return only the rows where payment_date is not null

Inv_Number                    The invoice_number column
Inv_Date                          The invoice_date column
Inv_Total                          The invoice_Total column
Date_of_payment           The payment_date
Total_payment                The payment_total */

SELECT invoice_number, invoice_date, invoice_total, payment_date, payment_total
FROM invoices
WHERE payment_date IS NOT NULL;
