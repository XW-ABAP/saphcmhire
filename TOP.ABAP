FUNCTION-POOL zDDD_hr_pafg_onboarding.      "MESSAGE-ID ..

* INCLUDE LZDDD_HR_PAFG_ONBOARDINGD...       " Local class definition


* importmant define begin
TYPES:tt_p0000 TYPE STANDARD TABLE OF p0000.
TYPES:tt_p0001 TYPE STANDARD TABLE OF p0001.
*TYPES:tt_p0021 TYPE TABLE OF p0021.
DATA:gt_return TYPE zDDD_hr_pa_tab_return,
     gv_massn  TYPE p0000-massn,
     gv_begda  TYPE p0000-begda,
     gv_pernr  TYPE p0000-pernr,
     gv_rfpnr  TYPE rfpnr,
     gv_massg  TYPE p0000-massg.
* importmant define end
