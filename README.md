# saphcmhire
sap hr hire
SAP HR global best practices with pa module interface
![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/8ddeab03-b6ca-4c5d-8d5c-2b428951c672)
Introduction:
Other systems write to the SAP PA system, which is first divided into onboarding, and then the operation of the information type
 
Operation List：
COP        Copy
DEL         Delete
DIS          Display
EDQ        Lock/unlock
INS          Create
LIS9        Delimit
MOD       Change
INSS       Create for Actions is not converted to Change
 
Programming schemes：
 
Step 1 Onboarding  The underlying configuration definition
  1A : Open sy-tcode  se11 create Transparent Table Z_XXXXX table  Undertake the type and version of the information  and se11 crate Maintenance View z_xxxxxx

![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/88be00fc-f2a7-4a76-ae6e-dfc39ed3edb7)




  1B: Open sy-tcode  se11 create Transparent Table Z_XXXXX table  Undertake the type and version of the information  and se11 crate Maintenance View z_xxxxxx


![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/464a7dc5-857e-4cfb-b5d0-d567bd9b3df1)



1C: Open sy-tcode  se11 create Transparent Table Z_XXXXX table  Undertake the type and version of the information  and se11 crate Maintenance View z_xxxxxx
![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/f4844480-ebdf-4356-811d-0c1bf9773a5f)


1D: open tcode se54 create View cluster ZXXXXXXX 
![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/fdc4c956-78e7-4e15-93cc-eb7137126dda)



1E: open sm34   We need to define SAP system fields to conform to DDIC and non-SAP system fields that have been converted to SAP values

Rewrite introduce
When a very special situation arises that requires a rewrite tick on this indication, the program will execute the change mode again
Some systems have a problem with t588m in the configuration table, causing the entire field to be readonly and unuse
![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/89f9fe53-f3f9-438f-b802-5d62f1a0eb2e)






Sap fieldname Need to be based on DDIC
No-sap fieldname  Please do not have special characters It is best to use 26 English letters plus numbers, taking special care not to completely duplicate the content
![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/4f17c3ae-9d15-455a-9a34-6bf00951c672)





Adding certain information type fields to an interface requires multiple assignments

Special note, when item key >= 9990,
The contents of target fielname replace the content corresponding to the Source Fieldname value

When  item key Values less than 5551 are from
Souce fieldname is transferred to Target Fielname


WHEN ITEM KEY >= 5551 AND  ITEM KEY <=7000

ITEM MOD 2 EQ 1  is what the field needs to replace

ITEM MOD 2 EQ 0 It is the content that the field needs to be replaced

![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/65d0e6fc-1162-450e-a5e9-9cc533fd8ef1)

![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/cfd44f8a-92ea-443f-a4dc-51528aa31e57)

![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/2539d484-f9df-4c76-8f51-c3422089396d)









![image](https://github.com/XW-bmw/saphcmhire/assets/29417134/ff358e90-e4c8-4a80-b4c9-595b6a0a6750)

