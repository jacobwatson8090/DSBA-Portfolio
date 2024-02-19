/* 1)
*/

SELECT invoice_number, invoice_date, invoice_total, payment_total + credit_total AS Payment_plus_Credit, 
invoice_total - (payment_total + credit_total) AS Balance_Due
FROM invoices
WHERE invoice_total - (payment_total + credit_total) BETWEEN 100 AND 1000;

/* 2) 
*/

SELECT vendor_id, concat(vendor_city, ', ', vendor_state) AS Vendor_city_state
FROM vendors
WHERE vendor_state REGEXP 'N[VY]';

/* 3) 
*/

SELECT invoice_number, DATE_FORMAT(invoice_date, '%e-%b-%Y') AS invoice_date, invoice_total
FROM invoices 
ORDER BY invoice_date DESC;

/* 4)
*/

SELECT invoice_number, invoice_date, invoice_total, payment_date, payment_total
FROM invoices
WHERE payment_date IS NOT NULL;