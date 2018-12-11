/*a*/

DATA Medicare_PS_PUF;
	LENGTH
		npi              					$ 10
		nppes_provider_last_org_name 		$ 70
		nppes_provider_first_name 			$ 20
		nppes_provider_mi					$ 1
		nppes_credentials 					$ 20
		nppes_provider_gender				$ 1
		nppes_entity_code 					$ 1
		nppes_provider_street1 				$ 55
		nppes_provider_street2				$ 55
		nppes_provider_city 				$ 40
		nppes_provider_zip 					$ 20
		nppes_provider_state				$ 2
		nppes_provider_country				$ 2
		provider_type 						$ 55
		medicare_participation_indicator 	$ 1
		place_of_service					$ 1
		hcpcs_code       					$ 5
		hcpcs_description 					$ 256
		hcpcs_drug_indicator				$ 1
		line_srvc_cnt      					8
		bene_unique_cnt    					8
		bene_day_srvc_cnt   				8
		average_Medicare_allowed_amt   		8
		average_submitted_chrg_amt  		8
		average_Medicare_payment_amt   		8
		average_Medicare_standard_amt		8;
	INFILE '.\data\Medicare_Provider_Util_Payment_PUF_CY2016.txt'

		lrecl=32767
		dlm='09'x
		pad missover
		firstobs = 3
		dsd;

	INPUT
		npi             
		nppes_provider_last_org_name 
		nppes_provider_first_name 
		nppes_provider_mi 
		nppes_credentials 
		nppes_provider_gender 
		nppes_entity_code 
		nppes_provider_street1 
		nppes_provider_street2 
		nppes_provider_city 
		nppes_provider_zip 
		nppes_provider_state 
		nppes_provider_country 
		provider_type 
		medicare_participation_indicator 
		place_of_service 
		hcpcs_code       
		hcpcs_description 
		hcpcs_drug_indicator
		line_srvc_cnt    
		bene_unique_cnt  
		bene_day_srvc_cnt 
		average_Medicare_allowed_amt 
		average_submitted_chrg_amt 
		average_Medicare_payment_amt
		average_Medicare_standard_amt;

	LABEL
		npi     							= "National Provider Identifier"       
		nppes_provider_last_org_name 		= "Last Name/Organization Name of the Provider"
		nppes_provider_first_name 			= "First Name of the Provider"
		nppes_provider_mi					= "Middle Initial of the Provider"
		nppes_credentials 					= "Credentials of the Provider"
		nppes_provider_gender 				= "Gender of the Provider"
		nppes_entity_code 					= "Entity Type of the Provider"
		nppes_provider_street1 				= "Street Address 1 of the Provider"
		nppes_provider_street2 				= "Street Address 2 of the Provider"
		nppes_provider_city 				= "City of the Provider"
		nppes_provider_zip 					= "Zip Code of the Provider"
		nppes_provider_state 				= "State Code of the Provider"
		nppes_provider_country 				= "Country Code of the Provider"
		provider_type	 					= "Provider Type of the Provider"
		medicare_participation_indicator 	= "Medicare Participation Indicator"
		place_of_service 					= "Place of Service"
		hcpcs_code       					= "HCPCS Code"
		hcpcs_description 					= "HCPCS Description"
		hcpcs_drug_indicator				= "Identifies HCPCS As Drug Included in the ASP Drug List"
		line_srvc_cnt    					= "Number of Services"
		bene_unique_cnt  					= "Number of Medicare Beneficiaries"
		bene_day_srvc_cnt 					= "Number of Distinct Medicare Beneficiary/Per Day Services"
		average_Medicare_allowed_amt 		= "Average Medicare Allowed Amount"
		average_submitted_chrg_amt 			= "Average Submitted Charge Amount"
		average_Medicare_payment_amt 		= "Average Medicare Payment Amount"
		average_Medicare_standard_amt		= "Average Medicare Standardized Payment Amount";
RUN;

/*b*/
data m1;
	set Medicare_PS_PUF;
	if find(hcpcs_description,'MRI','i') ge 1;
	if hcpcs_code =: '7';

proc print data=m1(obs=10);
	var hcpcs_code hcpcs_description; 
run;


/*c*/
data m2;
 	set m1;
	t_pay = average_Medicare_payment_amt*line_srvc_cnt;
   
proc summary data = m2;
  	class hcpcs_code;
  	output out=m3
   	sum(line_srvc_cnt) = volume
   	sum(t_pay) = total_pay;

data m4;
 	set m3;
	if _TYPE_=1;
	ave_pay = total_pay/volume;
run;

proc print data=m4;

proc means data = m4 max;
	var volume total_pay ave_pay;
run;
/*By this, we can get: 
highest volume :1430104.40 
highest total_pay :134223519 
highest ave_pay :269.1766460*/

data highest;
 set m4;
 if volume ge 1430104 OR total_pay ge 134223519 OR ave_pay ge 269;
 keep hcpcs_code volume total_pay ave_pay;

proc print data=highest;
run;

	
 /*d*/
/* Use proc sql:  */
proc sql;
  create table d1 as
    select *	 
      from Medicare_PS_PUF
      where hcpcs_code like '7%' and hcpcs_description like '%MRI%';

  /* Count total homes by state */
  create table highest_sql as
    select hcpcs_code, sum(line_srvc_cnt) as volume, 
		   sum(average_Medicare_payment_amt*line_srvc_cnt) as total_pay,
		   sum(average_Medicare_payment_amt*line_srvc_cnt)/sum(line_srvc_cnt) as ave_pay
      from d1
      group by hcpcs_code
	  having volume > 1430104 | total_pay > 134223519 | ave_pay>269;

  quit;

/* Print the result:*/
proc print data=highest_sql;
run;


/*e*/
/* Export to csv:  */
proc export data= highest
  outfile = './ps4_q3c.csv'
  dbms=dlm replace; 
  delimiter  = ",";

run; 

/* Export to csv: */
proc export data=highest_sql
  outfile = './ps4_q3d.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 
