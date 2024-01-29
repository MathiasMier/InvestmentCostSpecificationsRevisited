* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * Fundamentals
set
t                                Model time periods
tbase(t)                         Model base year
t2020(t)                         Model base + 1 period
v                                Vintages of generation capacity
vbase(v)                         First active vintage
oldv(v)                          Existing vintages
newv(v)                          New vintages
tv(t,v)                          Time period in which vintage v is installed
vt(v,t)                          Vintages active in time period t
r                                Model regions
cty                              Countries
xcty                             Map regions countries
;

$gdxin database\setpar_%n%.gdx
$load t, tbase, t2020, v, vbase, oldv, newv, tv, vt
$load r, cty, xcty
$gdxin

alias(r,rr);
alias(t,tt);
alias(v,vv);

* * * Timeseries
set
i                                Generation technology
h                                Hours
m                                Months
s                                Segments
hm(h,m)                          Map between hours and months for assembling availability factors
sm(s,m)                          Map between segments and months for assembling availability factors
hmaps(h,s)                       Map between hours and segments (which segment is for which real hour)
srep(s,h)                        Map from segments to representative (chosen) hours
peak(s,r)                        Peak segment
;

$gdxin database\setpar_%n%.gdx
$load i, h, m, s, hm, sm, hmaps, srep, peak=peak_s
$gdxin

parameter
hours(s)                 Number of hours per load segment
load(s,r)                Base year load across segments including both retail and direct (GW) (corrected)
load_s(s,r)              Base year load across segments including both retail and direct (GW) (uncorrected)
loadcorr(r)              Correction of load to meet demand
peakload(r)              Peakload in each region
minload(r)               Minload in each region
dref(r,t)                Indexed reference growth path
daref(r,t)               Reference annual demand by region over time (TWh)
daref_h(r)               Annual demand (hours)
daref_s(r)               Annual demand (segments)
daref_check(r,t)         Reference annual demand by region over time (TWh)
vrsc(s,i,r)              Capacity factor for variable resources (techboost yes no)
vrsc_s(s,i,r)            Capacity factor for variable resources (uncorrected)
vrsc_ss(s,i,r)            Capacity factor for variable resources (corrected)
vrsccorr(i,r)            Correction of wind to meet full-load hours
irnwflh_h(i,r)           Intermittent renewables full-load hours (hours)
irnwflh_s(i,r)           Intermittent renewables full-load hours (segments)
irnwflh_check(i,r)       Intermittent renewables full-load hours
number                   Number of segments
nyrs(t)                  Number of years
;

$gdxin database\setpar_%n%.gdx
$load hours, load_s, loadcorr, peakload=peakload_s, minload=minload_s, dref, daref, daref_h, daref_s
$load vrsc_s, vrsccorr, irnwflh_h, irnwflh_s, number, nyrs
$gdxin

* Correct time series to match annual load and full-load hours of renewables
$if      set corr_peak   vrsc_ss(s,i,r)$(vrsccorr(i,r) > 0)  = min(vrsccorr(i,r) * vrsc_s(s,i,r), 1) + eps ;
$if      set corr_peak   load(s,r)                           = min(loadcorr(r) * load_s(s,r), peakload(r)) + eps ;

$if      set corr_full   vrsc_ss(s,i,r)$(vrsccorr(i,r) > 0)  = vrsccorr(i,r) * vrsc_s(s,i,r) + eps ;
$if      set corr_full   load(s,r)                           = loadcorr(r) * load_s(s,r) + eps ;

$if not  set corr_full $if not set corr_peak     vrsc_ss(s,i,r)                      = vrsc_s(s,i,r) + eps ;
$if not  set corr_full $if not set corr_peak     load(s,r)                           = load_s(s,r) + eps ;

$if not  set notechboost vrsc(s,i,r) = vrsc_ss(s,i,r) ;
$if      set notechboost vrsc(s,i,r)$(not sameas(i,"WindOn_120m_high") or not sameas(i,"WindOn_120m_mid") or not sameas(i,"WindOn_120m_low") or
$if      set notechboost              not sameas(i,"WindOff_120m_high") or not sameas(i,"WindOff_120m_mid") or not sameas(i,"WindOff_120m_low")) = vrsc_ss(s,i,r) ;
$if      set notechboost vrsc(s,"WindOn_120m_high",r)  = vrsc_ss(s,"WindOn_100m_high",r)  ;
$if      set notechboost vrsc(s,"WindOn_120m_mid",r)   = vrsc_ss(s,"WindOn_100m_mid",r)   ;
$if      set notechboost vrsc(s,"WindOn_120m_low",r)   = vrsc_ss(s,"WindOn_100m_low",r)   ;
$if      set notechboost vrsc(s,"WindOff_120m_high",r) = vrsc_ss(s,"WindOff_100m_high",r) ;
$if      set notechboost vrsc(s,"WindOff_120m_mid",r)  = vrsc_ss(s,"WindOff_100m_mid",r)  ;
$if      set notechboost vrsc(s,"WindOff_120m_low",r)  = vrsc_ss(s,"WindOff_100m_low",r)  ;

daref_check(r,t)   = sum(s, hours(s) * load(s,r)) * dref(r,t) ;
irnwflh_check(i,r) = sum(s, hours(s) * vrsc(s,i,r)) ;

* * * Generation technology
set
new(i)                           New generation technology
exi(i)                           Existing technologies (or capacities) in base year - EXISTING BLOCKS
dspt(i)                          Dispatchable capacity blocks
ndsp(i)                          Non-Dispatchble capacity blocks (there are none of these in 4NEMO only price decides)
ccs(i)                           CCS generation technologies (or capacities) - CCS BLOCKS
irnw(i)                          Intermittent renewable technologies
sol(i)                           Solar technologies
wind(i)                          Wind technologies
rnw(i)                           Renewable technologies
lowcarb(i)                       Low-carbon technologies
nuc(i)                           Nuclear technologies
type                             Generation type
idef(i,type)                     Map between technology and type
iidef(i,type)                    Map between technology and type
class                            Intermittent renewables commodity classes for cap-up
xclass(class,i)                  Map class technology
;

$gdxin database\setpar_%n%.gdx
$load new, exi, dspt, ndsp, ccs, irnw, sol, wind, rnw, lowcarb, nuc, type, idef, class, xclass
$gdxin

iidef(i,type) = idef(i,type) ;

set
r_nuclim(v,r)                    Regions that retrict nuclear expansion
;

$gdxin database\setpar_%n%.gdx
$load r_nuclim
$gdxin

parameter
cap(i,v,r)                       Capacity installed by region (GW)
invlimUP(i,r,t)                  Upper bounds on investment based on potential (cumulative since last time period) (GW)
invlimLO(i,r,t)                  Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlimUP_eu(i,t)                 Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlife(i)                       Capacity lifetime
invdepr(i)                       Investment depreciation
capcost(i,v,r)                   Capacity cost (investment) by region
fomcost(i,v,r)                   Fixed OM cost
vomcost(i,v,r)                   Variable OM cost
effrate(i,v,r)                   Efficiency
co2captured(i,v,r)               CCS capture rate
emit(i,v,r)                      Emission factor
reliability(i,v,r)               Reliability factor by region and technology
capcred(i,v,r)                   Capacity credit by region and technology
mindisp(i,v,r)                   Min load by region and technology
invlimUP_irnw(r,class)           Upper bounds for intermittent renewables classes
sclim(r)                         Upper bound on geologic storage of carbon (GtCO2)
sclim_eu                         Upper bound on geologic storage of carbon (GtCO2)
biolim_int(r,t)                      Upper bounds by region on biomass use (MWh)
biolim_eu_int(t)                     Upper bounds by region on biomass use (MWh)
biolim(r,t)                      Upper bounds by region on biomass use (MWh)
biolim_eu(t)                     Upper bounds by region on biomass use (MWh)
;

$gdxin database\setpar_%n%.gdx
$load cap, invlimUP, invlimLO, invlimUP_eu, invlife, invdepr, capcost, fomcost, vomcost, effrate, co2captured, emit, reliability, capcred, mindisp, invlimUP_irnw
$load sclim, sclim_eu, biolim_int=biolim, biolim_eu_int=biolim_eu
$gdxin

$if not  set biolim            biolim_eu(t) = 1 * biolim_eu_int(t) ;
$if      set biolimnormal      biolim_eu(t) = 1 * biolim_eu_int(t) ;
$if      set biolimhalf        biolim_eu(t) = 0.5 * biolim_eu_int(t) ;
$if      set biolimdouble      biolim_eu(t) = 2 * biolim_eu_int(t) ;


$if not  set biolim_r          biolim(r,t) = 1 * biolim_int(r,t) ;
$if      set biolimnormal      biolim(r,t) = 1 * biolim_int(r,t) ;
$if      set biolimhalf        biolim(r,t) = 0.5 * biolim_int(r,t) ;
$if      set biolimdouble      biolim(r,t) = 2 * biolim_int(r,t) ;

* * * Technology rich
*MM (todo): Implement heuristic for the usage of technology rich model (not working in the moment)
set
i_rich                           Generation technology
new_rich(i_rich)                 New generation technology
exi_rich(i_rich)                 Existing technologies (or capacities) in base year - EXISTING BLOCKS
dspt_rich(i_rich)                Dispatchable capacity blocks
ndsp_rich(i_rich)                Non-Dispatchble capacity blocks (there are none of these in 4NEMO only price decides)
ccs_rich(i_rich)                 CCS generation technologies (or capacities) - CCS BLOCKS
idef_rich(i_rich,type)           Map between technology and type
retro(i_rich)                    Retrofit technologies
cofir(i_rich)                    Cofiring technologies
conve(i_rich)                    Conversion technologies
xretro(i_rich,i_rich,v)          Underlying capacity block (position 2) for retrofit (position 1)
xcofir(i_rich,i_rich,v)          Underlying capacity block (position 2) for cofiring (position 1)
xconve(i_rich,i_rich,v)          Underlying capacity block (position 2) for conversion (position 1)
;

parameter
xcapadj_retro
;

$gdxin database\setpar_%n%.gdx
$load i_rich, new_rich, exi_rich, dspt_rich, ndsp_rich, ccs_rich, idef_rich
$load retro, cofir, conve, xretro, xcofir, xconve
$gdxin

* * * Storage technology
set
j                                Storage technology
newj(j)                          New storage technology
exij(j)                          Existing storage technology
;

$gdxin database\setpar_%n%.gdx
$load j, newj, exij
$gdxin

parameter
gcap(j,v,r)                      Storage capacity by region (GW)
ghours(j,v,r)                    Hours of storage (room size relative to door size)
chrgpen(j,v,r)                   Charge efficiency penalty for storage by region (< 1)
dchrgpen(j,v,r)                  Discharge efficiency penalty for storage by region (< 1)
dischrg(j,v,r)                   Automatic storage discharge by region (in percent) (< 1)
gcapcost_int(j,v,r)              Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcost(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gfomcost(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gvomcost(j,v,r)                  Variable OM cost for storage by region (EUR per MWh)
greliability(j,v,r)              Storage reliability factor by region and technology
gcapcred(j,v,r)                  Storage capacity credit by region and technology
ginvlife(j,v)                    Storage investment life for new capacity additions (years)
ginvdepr(j,v)                    Storage investment life for new capacity additions (years)
ginvlimLO(j,r,t)                 Storage investment lower bound (GW)
ginvlimUP(j,r,t)                 Storage investment upper bound (GW)
ginvlimUP_eu(j,t)                Storage investment upper bound (GW)
;

$gdxin database\setpar_%n%.gdx
$load gcap, ghours, chrgpen, dchrgpen, dischrg, gcapcost_int=gcapcost, gfomcost, gvomcost, greliability, gcapcred, ginvlife, ginvdepr, ginvlimLO, ginvlimUP, ginvlimUP_eu
$gdxin

gcapcost(j,v,r)                                          = 1 * gcapcost_int(j,v,r) ;
$if      set doublegcc   gcapcost(j,v,r)$(v.val ge 2020) = 2 * gcapcost_int(j,v,r) ;
$if      set triplegcc   gcapcost(j,v,r)$(v.val ge 2020) = 3 * gcapcost_int(j,v,r) ;

* * * Transmission technology
set
transmission                    Transmission technologies
tmap_cty(transmission,cty,cty)  Countries eligible for transmission exchange
tmap(transmission,r,r)          Regions eligible for transmission exchange by technology
xtmap(r,r,cty,cty)              Map regions countries eligible for transmission exchange
;

$gdxin database\setpar_%n%.gdx
$load transmission, tmap, xtmap, tmap_cty
$gdxin

parameter
tcap(transmission,r,r)           Transmission capacity from region X to region Y (GW)
tcapcost(transmission,r,r)       Transmission investment cost ($ per kW)
tfomcost(transmission,r,r)       Fixed O&M cost of new transmision capacity (euro per kW-year)
tvomcost(transmission,r,r)       Variable O&M cost of new transmision capacity (euro per MWh)
trnspen(transmission,r,r)        Transmission loss penalty
tinvlimUP_int(transmission,r,r,t) Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimUP(transmission,r,r,t)    Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimLO(transmission,r,r,t)    Lower bound on total transmission capacity from region X to region Y (GW)
tinvlimUP_eu(transmission,t)     Lower bound on total transmission capacity from region X to region Y (GW)
tcapcred(transmission,r,r)       Capacity credit for transmisson by region and technology
tinvlife(transmission)           Capacity lifetime (years)
tinvdepr(transmission)           Investment depreciation (years)
;

$gdxin database\setpar_%n%.gdx
$load tcap, tcapcost, tfomcost, tvomcost, trnspen, tinvlimUP_int=tinvlimUP, tinvlimLO, tinvlimUP_eu, tcapcred, tinvlife, tinvdepr
$gdxin

tinvlimUP(transmission,r,rr,t)                                           = 1 * tinvlimUP_int(transmission,r,rr,t) ;
$if      set doublentc   tinvlimUP(transmission,r,rr,t)$(t.val ge 2035)  = 2 * tinvlimUP_int(transmission,r,rr,t) ;
$if      set triplentc   tinvlimUP(transmission,r,rr,t)$(t.val ge 2035)  = 3 * tinvlimUP_int(transmission,r,rr,t) ;

* * * Discounting
set
inv
share_sc
;

$gdxin database\setpar_%n%.gdx
$load inv,share_sc
$gdxin

parameter
share(inv,share_sc)              Share of investor type
* old parameters
tk                               Investment tax
drate                            Annual discount rate
irate                            Annual interest rate
dfact(t)                         Discount factor for time period t (reflects number of years) for both
annuity(i)                       Annuity factor for generation capacity
gannuity(j,v)                    Annuity factor for storage capacity
tannuity(transmission)           Annuity factor for transmission capacity
* new parameters for discounting
drate_normal                     Discount rate of normal investors
drate_annui                      Discount rate of annui investors
drate_ccost                      Discount rate of wacc investors
drate_weight(share_sc)           Discount rate weighted over all investors by share scenario
dfact_weight(t,share_sc)         Discount factor weighted over all investors by share scenario
* new parameters for ccost
irate_ccost                      Interest of wacc investors
iratei(i)                        Interest rate by generation technology
iratej(j)                        Interest rate by storage technology
iratet(transmission)             Interest rate by transmission technology
iratei_ccost(i)                  Interest rate of ccost investors by generation technology
iratej_ccost(j)                  Interest rate of ccost investors by storage technology
iratet_ccost(transmission)       Interest rate of ccost investors by transmission technology
* new parameters for annui
annuity_annui(i)                 Annuity factor for generation capacity
gannuity_annui(j,v)              Annuity factor for storage capacity
tannuity_annui(transmission)     Annuity factor for transmission capacity
annuityi(i)                      Annuity factor for generation capacity
gannuityj(j,v)                   Annuity factor for storage capacity
tannuityt(transmission)          Annuity factor for transmission capacity
annuityi_annui(i)                Annuity factor for generation capacity
gannuityj_annui(j,v)             Annuity factor for storage capacity
tannuityt_annui(transmission)    Annuity factor for transmission capacity
;

$gdxin database\setpar_%n%.gdx
$load tk, drate, irate, dfact, annuity, gannuity, tannuity
$load share,drate_normal,drate_annui,drate_ccost,drate_weight,dfact_weight
$load irate_ccost,iratei,iratej,iratet,iratei_ccost,iratej_ccost,iratet_ccost
$load annuity_annui,gannuity_annui,tannuity_annui,annuityi,gannuityj,tannuityt,annuityi_annui,gannuityj_annui,tannuityt_annui
$gdxin

* Investors module
Parameter
sharenormal                      Share of normal investors
shareannui                       Share of annui investors
shareccost                       Share of wacc investors
dfact_model(t)                   Discount factor weighted over all investors by share scenario
irate_model(i)
girate_model(j)
tirate_model(transmission)
annuity_model(i)
gannuity_model(j,v)
tannuity_model(transmission)
;

$if      set sharemixed       sharenormal = share("normal","mixed_sc") ;
$if      set sharemixed       shareannui  = share("annui","mixed_sc") ;
$if      set sharemixed       shareccost  = share("ccost","mixed_sc") ;

$if      set sharenormal      sharenormal = share("normal","normal_sc") ;
$if      set sharenormal      shareannui  = share("annui","normal_sc") ;
$if      set sharenormal      shareccost  = share("ccost","normal_sc") ;

$if      set shareannui       sharenormal = share("normal","annui_sc") ;
$if      set shareannui       shareannui  = share("annui","annui_sc") ;
$if      set shareannui       shareccost  = share("ccost","annui_sc") ;

$if      set shareccost       sharenormal = share("normal","ccost_sc") ;
$if      set shareccost       shareannui  = share("annui","ccost_sc") ;
$if      set shareccost       shareccost  = share("ccost","ccost_sc") ;



$if      set sharemixed       dfact_model(t) = dfact_weight(t,"mixed_sc") ;
$if      set sharenormal      dfact_model(t) = dfact_weight(t,"normal_sc") ;
$if      set shareannui       dfact_model(t) = dfact_weight(t,"annui_sc") ;
$if      set shareccost       dfact_model(t) = dfact_weight(t,"ccost_sc") ;

$if      set classicdisc      dfact_model(t) = dfact(t) ;
$if      set nodisc           dfact_model(t) = nyrs(t) ;

* Annuity and drate switches
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       annuity_model(i) = annuity(i) ;
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       gannuity_model(j,v) = gannuity(j,v) ;
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       tannuity_model(transmission) = tannuity(transmission) ;
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       irate_model(i) = irate ;
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       girate_model(j)  = irate;
$if not  set vardfact    $if not set vardfacti       $if not set vardfactii       tirate_model(transmission) = irate ;

$if      set vardfact                            annuity_model(i)                = annuity_annui(i) ;
$if      set vardfact                            gannuity_model(j,v)             = gannuity_annui(j,v) ;
$if      set vardfact                            tannuity_model(transmission)    = tannuity_annui(transmission) ;
$if      set vardfact                            irate_model(i)                  = irate_ccost ;
$if      set vardfact                            girate_model(j)                 = irate_ccost;
$if      set vardfact                            tirate_model(transmission)      = irate_ccost ;

$if      set vardfacti                           annuity_model(i)                = annuityi(i) ;
$if      set vardfacti                           gannuity_model(j,v)             = gannuityj(j,v) ;
$if      set vardfacti                           tannuity_model(transmission)    = tannuityt(transmission) ;
$if      set vardfacti                           irate_model(i)                  = iratei(i) ;
$if      set vardfacti                           girate_model(j)                 = iratej(j) ;
$if      set vardfacti                           tirate_model(transmission)      = iratet(transmission) ;

$if      set vardfactii                          annuity_model(i)                = annuityi_annui(i) ;
$if      set vardfactii                          gannuity_model(j,v)             = gannuityj_annui(j,v) ;
$if      set vardfactii                          tannuity_model(transmission)    = tannuityt_annui(transmission) ;
$if      set vardfactii                          irate_model(i)                  = iratei_ccost(i) ;
$if      set vardfactii                          girate_model(j)                 = iratej_ccost(j) ;
$if      set vardfactii                          tirate_model(transmission)      = iratet_ccost(transmission) ;

* * * Lifetime and depreciation
parameter
lifetime(i,v,r,t)                Lifetime coefficient for existing and new capacity
deprtime(i,v,r,t)                Depreciation coefficient for existing and new capacity
glifetime(j,v,r,t)               Lifetime coefficient for existing and new capacity
gdeprtime(j,v,r,t)               Depreciation coefficient for existing and new capacity
tlifetime(transmission,v,t)      Depreciation coefficient for existing and new capacity
tdeprtime(transmission,v,t)      Fraction of discounted annualized payment stream contained in remaining model time horizon
endeffect(i,v,r,t)               Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gendeffect(j,v,r,t)              Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tendeffect(transmission,v,t)     Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
modeldepr(i,v,r,t)               Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gmodeldepr(j,v,r,t)              Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tmodeldepr(transmission,v,t)     Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
modeldepr_nodisc(i,v,r,t)        Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gmodeldepr_nodisc(j,v,r,t)       Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tmodeldepr_nodisc(transmission,v,t) Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
modeldepr_normal(i,v,r,t)                                  Fraction of discounted annualized payment stream contained in remaining model time horizon
gmodeldepr_normal(j,v,r,t)                                 Fraction of discounted annualized payment stream contained in remaining model time horizon
tmodeldepr_normal(transmission,v,t)                        Fraction of discounted annualized payment stream contained in remaining model time horizon
modeldepri(i,v,r,t)                                        Fraction of discounted annualized payment stream contained in remaining model time horizon
gmodeldeprj(j,v,r,t)                                       Fraction of discounted annualized payment stream contained in remaining model time horizon
tmodeldeprt(transmission,v,t)                              Fraction of discounted annualized payment stream contained in remaining model time horizon
modeldepri_normal(i,v,r,t)                                  Fraction of discounted annualized payment stream contained in remaining model time horizon
gmodeldeprj_normal(j,v,r,t)                                 Fraction of discounted annualized payment stream contained in remaining model time horizon
tmodeldeprt_normal(transmission,v,t)                        Fraction of discounted annualized payment stream contained in remaining model time horizon
;

$gdxin database\setpar_%n%.gdx
$load lifetime, deprtime
$load glifetime, gdeprtime
$load tlifetime, tdeprtime
$load modeldepr, modeldepr_nodisc
$load gmodeldepr, gmodeldepr_nodisc
$load tmodeldepr, tmodeldepr_nodisc
$load modeldepr_normal,gmodeldepr_normal,tmodeldepr_normal,modeldepri,gmodeldeprj,tmodeldeprt,modeldepri_normal,gmodeldeprj_normal,tmodeldeprt_normal

$if not  set myopic    $if not set vardfact    $if not set vardfacti    $if not set vardfactii    $load endeffect=modeldepr
$if not  set myopic    $if not set vardfact    $if not set vardfacti    $if not set vardfactii    $load gendeffect=gmodeldepr
$if not  set myopic    $if not set vardfact    $if not set vardfacti    $if not set vardfactii    $load tendeffect=tmodeldepr

* MM (todo): Myopic version does not have varying discount rates options
$if      set myopic    $if set overlap1        $load endeffect=modeldepr_myopic

$if      set myopic    $if set overlap2        $load endeffect=modeldepr_myopic_overlap2

$if      set myopic    $if set overlap3        $load endeffect=modeldepr_myopic_overlap3

$if      set myopic    $if set overlap4        $load endeffect=modeldepr_myopic_overlap4

$if      set myopic    $if set overlap5        $load endeffect=modeldepr_myopic_overlap5

$if      set myopic    $if set overlap6        $load endeffect=modeldepr_myopic_overlap6

$if      set myopic    $if set overlap7        $load endeffect=modeldepr_myopic_overlap7

$if      set myopic    $if set overlap8        $load endeffect=modeldepr


$if      set myopic    $if set overlap1        $load gendeffect=gmodeldepr_myopic

$if      set myopic    $if set overlap2        $load gendeffect=gmodeldepr_myopic_overlap2

$if      set myopic    $if set overlap3        $load gendeffect=gmodeldepr_myopic_overlap3

$if      set myopic    $if set overlap4        $load gendeffect=gmodeldepr_myopic_overlap4

$if      set myopic    $if set overlap5        $load gendeffect=gmodeldepr_myopic_overlap5

$if      set myopic    $if set overlap6        $load gendeffect=gmodeldepr_myopic_overlap6

$if      set myopic    $if set overlap7        $load gendeffect=gmodeldepr_myopic_overlap7

$if      set myopic    $if set overlap8        $load gendeffect=gmodeldepr


$if      set myopic    $if set overlap1        $load tendeffect=tmodeldepr_myopic

$if      set myopic    $if set overlap2        $load tendeffect=tmodeldepr_myopic_overlap2

$if      set myopic    $if set overlap3        $load tendeffect=tmodeldepr_myopic_overlap3

$if      set myopic    $if set overlap4        $load tendeffect=tmodeldepr_myopic_overlap4

$if      set myopic    $if set overlap5        $load tendeffect=tmodeldepr_myopic_overlap5

$if      set myopic    $if set overlap6        $load tendeffect=tmodeldepr_myopic_overlap6

$if      set myopic    $if set overlap7        $load tendeffect=tmodeldepr_myopic_overlap7

$if      set myopic    $if set overlap8        $load tendeffect=tmodeldepr

$gdxin


$if not  set myopic    $if set vardfact        endeffect(i,v,r,t)=modeldepr_normal(i,v,r,t) ;
$if not  set myopic    $if set vardfact        gendeffect(j,v,r,t)=gmodeldepr_normal(j,v,r,t) ;
$if not  set myopic    $if set vardfact        tendeffect(transmission,v,t)=tmodeldepr_normal(transmission,v,t) ;

$if not  set myopic    $if set vardfacti       endeffect(i,v,r,t)=modeldepri(i,v,r,t) ;
$if not  set myopic    $if set vardfacti       gendeffect(j,v,r,t)=gmodeldeprj(j,v,r,t)  ;
$if not  set myopic    $if set vardfacti       tendeffect(transmission,v,t)=tmodeldeprt(transmission,v,t) ;

$if not  set myopic    $if set vardfactii      endeffect(i,v,r,t)=modeldepri_normal(i,v,r,t) ;
$if not  set myopic    $if set vardfactii      gendeffect(j,v,r,t)=gmodeldeprj_normal(j,v,r,t) ;
$if not  set myopic    $if set vardfactii      tendeffect(transmission,v,t)=tmodeldeprt_normal(transmission,v,t) ;

* * * Adjust lifetime in accordance to German nuclear exit (1970 vintages is active in 2015 but not 2020, in 2025 0.3 of German nuclear power is still active)
$if      set gernucexit     $if not set central  lifetime("Nuclear","1970",r,t)$((t.val ge 2020) and sameas(r,"Germany")) = 0 ;
$if      set gernucexit     $if not set central  lifetime("Nuclear","1980",r,t)$((t.val ge 2025) and sameas(r,"Germany")) = 0 ;
$if      set gernucexit     $if not set central  lifetime("Nuclear","1985",r,"2025")$(sameas(r,"Germany")) = 0.3 ;
$if      set gernucexit     $if not set central  lifetime("Nuclear","1985",r,t)$((t.val ge 2030) and sameas(r,"Germany")) = 0 ;

$if      set gernucexit     $if     set central  lifetime("Nuclear","1970",r,"2020")$(sameas(r,"Central")) = 0.41 ;
$if      set gernucexit     $if     set central  lifetime("Nuclear","1970",r,"2025")$(sameas(r,"Central")) = 0.41 ;
$if      set gernucexit     $if     set central  lifetime("Nuclear","1985",r,"2025")$(sameas(r,"Central")) = 0.3 ;
$if      set gernucexit     $if     set central  lifetime("Nuclear","1980",r,t)$((t.val ge 2025) and sameas(r,"Central")) = 0 ;
$if      set gernucexit     $if     set central  lifetime("Nuclear","1985",r,t)$((t.val ge 2030) and sameas(r,"Central")) = 0 ;

* * * Prices
set
fuel                             Fuel
xfueli(fuel,i)                   Map fuel technology
;

$gdxin database\setpar_%n%.gdx
$load fuel, xfueli
$gdxin

parameter
pfuel(fuel,r,t)          Fuel price (EUR er MWh)
pfadd(fuel,r,t)          Absolute fuel price adders (EUR per MWh)
pfadd_rel(fuel,r,t)      Relative fuel price adders (value between 0 (no adding) and x)
pco2_r(r,t)              CO2 price by region (EUR per tCO2)
pco2_int(t)              CO2 price (EUR per tCO2) standard
pco2(t)                  CO2 price (EUR per tCO2)
biocost(r,t)             Bioenegy cost (EUR per MWh)
biocost_eu(t)            Bioenegy cost (EUR per MWh)
ccscost(r,t)             CCS CO2 transportation cost (EUR per tCO2)
ccscost_eu(t)            CCS CO2 transportation cost (EUR per tCO2)
;

$gdxin database\setpar_%n%.gdx
$load pfuel, pfadd, pfadd_rel, pco2_r, pco2_int=pco2, biocost, biocost_eu, ccscost, ccscost_eu
$gdxin

pco2(t)                                          = 1 * pco2_int(t) ;
$if      set flatpco2    pco2(t)$(t.val ge 2025) = 1 * pco2_int("2020") ;
$if      set doublepco2  pco2(t)$(t.val ge 2025) = 2 * pco2_int(t) ;
$if      set triplepco2  pco2(t)$(t.val ge 2025) = 3 * pco2_int(t) ;

* * *  Calibration
parameter
loss_tot(r)              Distribution loss by region (TWh)
loss(r)                  Distribution loss by region (% of demand)
monthshape(m)            Monthly availability shape
af_mon(m,i,r)            Monthly availability by region
voll(r,t)                Value of lost load (EUR per MWh)
;

$gdxin database\setpar_%n%.gdx
$load loss_tot, loss, monthshape, af_mon, voll
$gdxin


* * * Optional modules to activate when analyzing certain research questions

* * * Learning

* * * R&D

* * * Spillover

* * * Policy
set
pol_sc                   Defines a policy scenario
co2cap_sc                Defines a carbon cap scenario
rnwtgt_sc                Defines a renewable energy share (absolute value) target scenario
irnwtgt_sc               Defines a intermittent renewable energy share (absolute value) target scenario
coalexit_sc              Defines a coalexit scenario
nucexit_sc               Defines a nuclear exit scenario
;

$gdxin database\setpar_%n%.gdx
$load pol_sc, co2cap_sc, rnwtgt_sc, irnwtgt_sc, coalexit_sc, nucexit_sc
$gdxin

parameter
co2p_int(pol_sc,t)               Carbon price (EUR per t) (system)
co2cap_r_int(co2cap_sc,r,t)      Carbon emissions cap (GtCO2) (regions)
co2cap_int(pol_sc,t)             Carbon emissions cap (GtCO2) (system)
rnwtgt_r_int(rnwtgt_sc,r,t)      Renewable energy share target (regions)
rnwtgt_int(rnwtgt_sc,t)          Renewable energy share target (system)
irnwtgt_r_int(irnwtgt_sc,r,t)    Intermittent renewable energy share target (regions)
irnwtgt_int(irnwtgt_sc,t)        Intermittent renewable energy share target (system)
coallim_r_int(coalexit_sc,r,t)   Policy constraint on hard coal phase out (regions)
coallim_int(coalexit_sc,t)       Policy constraint on hard coal phase out (system)
lignlim_r_int(coalexit_sc,r,t)   Policy constraint on lignite phase out (regions)
lignlim_int(coalexit_sc,t)       Policy constraint on lignite phase out (system)
nuclim_r_int(nucexit_sc,r,t)     Policy constraint on nuclear phase out (regions)
nuclim_int(nucexit_sc,t)         Policy constraint on nuclear phase out (system)

co2p(t)                          Carbon price (EUR per t) (system)
co2cap_r(r,t)                    Carbon emissions cap (GtCO2) (regions)
co2cap(t)                        Carbon emissions cap (GtCO2) (system)
rnwtgt_r(r,t)                    Renewable energy share target (regions)
rnwtgt(t)                        Renewable energy share target (system)
irnwtgt_r(r,t)                   Intermittent renewable energy share target (regions)
irnwtgt(t)                       Intermittent renewable energy share target (system)
coallim_r(r,t)                   Policy constraint on hard coal phase out (regions)
coallim(t)                       Policy constraint on hard coal phase out (system)
lignlim_r(r,t)                   Policy constraint on lignite phase out (regions)
lignlim(t)                       Policy constraint on lignite phase out (system)
nuclim_r(r,t)                    Policy constraint on nuclear phase out (regions)
nuclim(t)                        Policy constraint on nuclear phase out (system)
;

$gdxin database\setpar_%n%.gdx
$load co2cap_r_int=co2cap_r, co2p_int=co2p, co2cap_int=co2cap, rnwtgt_r_int=rnwtgt_r, rnwtgt_int=rnwtgt, irnwtgt_r_int=irnwtgt_r, irnwtgt_int=irnwtgt
$load coallim_int=coallim, coallim_r_int=coallim_r, lignlim_int=lignlim, lignlim_r_int=lignlim_r, nuclim_int=nuclim, nuclim_r_int=nuclim_r
$gdxin

* Scenario switches
$if      set euetsmsr    co2cap(t) = co2cap_int("euetsmsr",t) ;
$if      set greendeal   co2cap(t) = co2cap_int("greendeal",t) ;
$if      set euetsmsrnt    co2cap(t) = co2cap_int("euetsmsrnt",t) ;
$if      set greendealnt   co2cap(t) = co2cap_int("greendealnt",t) ;
$if      set faust         co2cap(t) = co2cap_int("faust",t) ;

$if      set euetsmsr    co2p(t) = co2p_int("euetsmsr",t) ;
$if      set greendeal   co2p(t) = co2p_int("greendeal",t) ;
$if      set euetsmsrnt    co2p(t) = co2p_int("euetsmsrnt",t) ;
$if      set greendealnt   co2p(t) = co2p_int("greendealnt",t) ;
$if      set faust         co2p(t) = co2p_int("faust",t) ;

$if      set ger         coallim_r(r,t) = coallim_r_int("ger",r,t) ;
$if      set ger-fast    coallim_r(r,t) = coallim_r_int("ger-fast",r,t) ;
$if      set ger-slow    coallim_r(r,t) = coallim_r_int("ger-slow",r,t) ;
$if      set eu          coallim_r(r,t) = coallim_r_int("eu",r,t) ;
$if      set eu-fast     coallim_r(r,t) = coallim_r_int("eu-fast",r,t) ;
$if      set eu-slow     coallim_r(r,t) = coallim_r_int("eu-flow",r,t) ;

$if      set ger         lignlim_r(r,t) = lignlim_r_int("ger",r,t) ;
$if      set ger-fast    lignlim_r(r,t) = lignlim_r_int("ger-fast",r,t) ;
$if      set ger-slow    lignlim_r(r,t) = lignlim_r_int("ger-slow",r,t) ;
$if      set eu          lignlim_r(r,t) = lignlim_r_int("eu",r,t) ;
$if      set eu-fast     lignlim_r(r,t) = lignlim_r_int("eu-fast",r,t) ;
$if      set eu-slow     lignlim_r(r,t) = lignlim_r_int("eu-flow",r,t) ;

$if      set ger         nuclim_r(r,t) = nuclim_r_int("ger",r,t) ;
$if      set ger-fast    nuclim_r(r,t) = nuclim_r_int("ger-fast",r,t) ;
$if      set ger-slow    nuclim_r(r,t) = nuclim_r_int("ger-slow",r,t) ;
$if      set eu          nuclim_r(r,t) = nuclim_r_int("eu",r,t) ;
$if      set eu-fast     nuclim_r(r,t) = nuclim_r_int("eu-fast",r,t) ;
$if      set eu-slow     nuclim_r(r,t) = nuclim_r_int("eu-flow",r,t) ;

$if      set aaa         co2cap_r(r,t) = co2cap_r_int("aaa",r,t) ;
$if      set bbb         co2cap_r(r,t) = co2cap_r_int("bbb",r,t) ;
$if      set ccc         co2cap_r(r,t) = co2cap_r_int("ccc",r,t) ;

$if      set aaa         rnwtgt_r(r,t) = rnwtgt_r_int("aaa",r,t) ;
$if      set bbb         rnwtgt_r(r,t) = rnwtgt_r_int("bbb",r,t) ;
$if      set ccc         rnwtgt_r(r,t) = rnwtgt_r_int("ccc",r,t) ;

$if      set aaa         irnwtgt_r(r,t) = irnwtgt_r_int("aaa",r,t) ;
$if      set bbb         irnwtgt_r(r,t) = irnwtgt_r_int("bbb",r,t) ;
$if      set ccc         irnwtgt_r(r,t) = irnwtgt_r_int("ccc",r,t) ;

$if      set ger         coallim(t) = coallim_int("ger",t) ;
$if      set ger-fast    coallim(t) = coallim_int("ger-fast",t) ;
$if      set ger-slow    coallim(t) = coallim_int("ger-slow",t) ;
$if      set eu          coallim(t) = coallim_int("eu",t) ;
$if      set eu-fast     coallim(t) = coallim_int("eu-fast",t) ;
$if      set eu-slow     coallim(t) = coallim_int("eu-flow",t) ;

$if      set ger         lignlim(t) = lignlim_int("ger",t) ;
$if      set ger-fast    lignlim(t) = lignlim_int("ger-fast",t) ;
$if      set ger-slow    lignlim(t) = lignlim_int("ger-slow",t) ;
$if      set eu          lignlim(t) = lignlim_int("eu",t) ;
$if      set eu-fast     lignlim(t) = lignlim_int("eu-fast",t) ;
$if      set eu-slow     lignlim(t) = lignlim_int("eu-flow",t) ;

$if      set ger         nuclim(t) = nuclim_int("ger",t) ;
$if      set ger-fast    nuclim(t) = nuclim_int("ger-fast",t) ;
$if      set ger-slow    nuclim(t) = nuclim_int("ger-slow",t) ;
$if      set eu          nuclim(t) = nuclim_int("eu",t) ;
$if      set eu-fast     nuclim(t) = nuclim_int("eu-fast",t) ;
$if      set eu-slow     nuclim(t) = nuclim_int("eu-flow",t) ;

*$if      set aaa         co2cap(t) = co2cap_int("aaa",t) ;
*$if      set bbb         co2cap(t) = co2cap_int("bbb",t) ;
*$if      set ccc         co2cap(t) = co2cap_int("ccc",t) ;

$if      set aaa         rnwtgt(t) = rnwtgt_int("aaa",t) ;
$if      set bbb         rnwtgt(t) = rnwtgt_int("bbb",t) ;
$if      set ccc         rnwtgt(t) = rnwtgt_int("ccc",t) ;

$if      set aaa         irnwtgt(t) = irnwtgt_int("aaa",t) ;
$if      set bbb         irnwtgt(t) = irnwtgt_int("bbb",t) ;
$if      set ccc         irnwtgt(t) = irnwtgt_int("ccc",t) ;

* * * market_out
set
ngclass          Natural gas supply classes
dbclass          Dedicated biomass supply classes
;

$gdxin database\setpar_%n%.gdx
$load ngclass, dbclass
$gdxin

parameter
ngref_r(r,t)                     Reference natural gas consumption (EJ) (regional)
ngref(t)                         Reference natural gas consumption (EJ) (system)
ngelas_r(r,t)                    Supply elasticity for natural gas (regional)
ngelas(t)                        Supply elasticity for natural gas (system)
ngcost_r(ngclass,r,t)            Cost of natural gas by supply class (EUR per MWh th) (regional)
ngcost(ngclass,t)                Cost of natural gas by supply class (EUR per MWh th) (system)
nglim_r(ngclass,r,t)             Class size of natural gas supply function (regional)
nglim(ngclass,t)                 Class size of natural gas supply function (system)
dbref_r(r,t)                     Reference dedicated biomass consumption (EJ) (regional)
dbref(t)                         Reference dedicated biomass consumption (EJ) (system)
dbelas_r(r,t)                    Supply elasticity for dedicated biomass (regional)
dbelas(t)                        Supply elasticity for dedicated biomass (system)
dbcost_r(dbclass,r,t)            Cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbcost(dbclass,t)                Cost of dedicated biomass by supply class (EUR per MWh th) (system)
dblim_r(dbclass,r,t)             Class size of dedicated biomass supply function (regional)
dblim(dbclass,t)                 Class size of dedicated biomass supply function (system)
;

$gdxin database\setpar_%n%.gdx
$load ngref, ngref_r, ngelas, ngelas_r, ngcost, ngcost_r, nglim, nglim_r
$load dbref, dbref_r, dbelas, dbelas_r, dbcost, dbcost_r, dblim, dblim_r
$gdxin

* * * Demand module
set
l                        Load or demand sector
sdclass                  Short-term demand supply classes
d                        Clustering dimensions
elasticity_sc            Elasticity scenario
;

$gdxin database\setpar_%n%.gdx
$load l, sdclass, d, elasticity_sc
$gdxin

parameter
daref_sec(r,t,l)        Sector demand (TWh)
pelas(r,t)              Price elasticity at reference point (a negative value)
paref(r,t)              Reference annual average price in euro per MWh
cb_1(r,t)               Consumer benefit linear coefficient
cb_2(r,t)               Consumer benefit quadratic coefficient
;

$gdxin database\setpar_%n%.gdx
$load daref_sec=daref_sec_r
$gdxin

* Demand function calibration
* Define consumer benefit based on a linear demand curve that has a negative elasticity
* pelas(r) at the point (daref,paref).  Consumer benefit for demand x is equal to
* the integral of the inverse demand function P(x) from 0 to x, where
* P(x) = paref + (1/pelas)*(paref/daref)*(x - daref).
* If we state the integral of P(x) as cb_1*x + 0.5*cb_2*x^2,
* the appropriate coefficients are as follows:

pelas(r,t) = 0;
paref(r,t) = 0;

cb_1(r,t)$pelas(r,t) = paref(r,t) * (1 - 1/pelas(r,t));
cb_2(r,t)$pelas(r,t) = (1/pelas(r,t))*(paref(r,t)/daref(r,t));

* * * Energy efficiency module

* * * Social cost module
set
ap                               Air pollutant
impactap                         Impact of air pollutant
emfap_sc                         Scenario emission factor ;

*$gdxin database\setpar_%n%.gdx
*$load ap, impactap, emfap_sc
*$gdxin

parameter
gdpgrowth(r,t)                                   GDP growth index
gdpdistri(r,t)                                   GDP distributional index
emfap(i,emfap_sc,ap,v)                           Emission factor air pollutant (t per MWh thermal)
emitap(i,emfap_sc,ap,v,r)                        Emission factor air pollutant (t per MWh electric)
scap(ap,impactap,r,t)                            Social cost of air pollution (2015er EUR per t)
scap_emit_impactap(impactap,emfap_sc,i,v,r,t)    Social cost of air pollution by impact (2015er EUR per MWh electric)
scap_emit_ap(ap,emfap_sc,i,v,r,t)                Social cost of air pollution by pollutant (2015er EUR per MWh electric)
scap_emit(emfap_sc,i,v,r,t)                      Social cost of air pollution (2015er EUR per MWh electric)
scc(t)                                           Social cost of carbon (2015er EUR per t)
scc_emit(i,v,r,t)                                Social cost of carbon (2015er EUR per MWh electric) ;

*$gdxin database\setpar_%n%.gdx
*$load gdpgrowth, gdpdistri, emfap, emitap, scap, scap_emit_impactap, scap_emit_ap, scap_emit, scc, scc_emit
*$gdxin

* * * EUETS and MSR
parameter
euets_supply(t)                  EU ETS supplied allowances (MtCO2)
nbc_start(t)                     Used bank start value (MtCO2) (to initialize base year)
nbc_low(t)                       Used bank lower value (MtCO2)
nbc_up(t)                        Used bank upper value (MtCO2)
co2elec_start(t)                 Used allowances from electricity generation start value (MtCO2) (to initialize base year)
co2elec_low(t)                   Used allowances from electricity generation lower value (MtCO2)
co2elec_up(t)                    Used allowances from electricity generation upper value (MtCO2)
excess_start(t)                  Excess allowances at the start of the trading period (MtCO2)
excess_end(t)                    Excess allowances at the end of the trading period (MtCO2)
cumbank_start(t)                 Cumulative banked emissions at the start of the trading period (MtCO2)
cumbank_end(t)                   Cumulative banked emissions at the end of the trading period (MtCO2)
tnac_start(t)                    Total number of allowances in circulation at the start of the trading period (MtCO2)
tnac_end(t)                      Total number of allowances in circulation at the end of the trading period (MtCO2)
msr_flow(t)                      Flow of allowances into (positive) or from (negative) the market stability reserve (MSR) (MtCO2)
msr_level(t)                     Level of allowances in MSR (MtCO2)
msr_cancel(t)                    Cancelled allowances from MSR (MtCO2)
co2ind_scale(t)                  Scale of industry CO2 emissions
co2ind_abs(t)                    Absolute industry CO2 emissions
;

$if      set euets       $gdxin euets_corona\euets_simulation_%d%_%se%
$if      set euets       $load euets_suppy, nbc_start, nbc_low, nbc_up, co2elec_start, co2elec_low, co2elec_up, excess_start, excess_end, cumbank_start, cumbank_end, tnac_start, tnac_end, msr_flow, msr_level, msr_cancel, co2ind_scale, co2ind_abs
$if      set euets       $gdxin

* * * Adjustments according to Corona crisis
parameter
demandelec_scale(r,t)            Scale of electricity demand (factor between 0 and 1)
pricefuel_scale(t,fuel)          Scale of fuel price
;

* Correct 2019 load to set it equal to 2020 load
$if      set corona_load dref(r,"2015") = dref(r,"2020") ;
$if      set corona_load daref(r,"2015") = daref(r,"2020") ;

* Correct investment limits for 2019 according to pipeline (only for the sce0 investments)
$if      set corona_sce0 invlimLO(i,r,"2015") = invlimLO(i,r,"2020") ;
$if      set corona_sce0 invlim(i,r,"2015")   = invlimLO(i,r,"2020") ;
$if      set corona_sce0 invlimLO(i,r,"2020") = 0 ;

* Load corona investment limits and data from sce0 investments
$if      set corona_flim $gdxin corona_limits_sce0
$if      set corona_flim $load invlimUP = invlimUP_int, invlimLO = invlimLO_int, tinvlimUP = tinvlimUP_int, tinvlimLO = tinvlimLO_int, ginvlimUP = ginvlimUP_int, ginvlimLO = ginvlimLO_int
$if      set corona_flim $gdxin

* Set 2019 CO2 price according to real world
$if      set corona_pco2 pco2(t)$(t.val = 2015) =  24.65 ;
$if      set corona_pco2 pco2(t)$(t.val > 2015) =  0 ;

$if      set corona      $gdxin euets_corona\euets_simulation_%d%_%se%
$if      set corona      $load demandelec_scale, pricefuel_scale
$if      set corona      $gdxin

* * * Subsidies and taxes (negative subsidies are taxes)
parameter
irnwsub(r,t)                     New irnw production subsidy (EUR per MWh)
rnwsub(r,t)                      New rnw production subsidy (EUR per MWh)
solsub(r,t)                      New solar PV production subsidy in (EUR per MWh)
windsub(r,t)                     New wind production subsidy (EUR per MWh)
nucsub(r,t)                      New nuclear production subsidy (EUR per MWh)
lowcarbsub(r,t)                  New rnw nuc and CCS production subsidy (EUR per MWh)
irnwsub_cap(r,t)                 New irnw capacity subsidy (EUR per kW)
rnwsub_cap(r,t)                  New rnw capacity subsidy (EUR per kW)
solsub_cap(r,t)                  New solar PV capacity subsidy in (EUR per kW)
windsub_cap(r,t)                 New wind capacity subsidy (EUR per kW)
nucsub_cap(r,t)                  New nuclear capacity subsidy (EUR per kW)
lowcarbsub_cap(r,t)              New rnw nuc and CCS capacity subsidy (EUR per kW)
taxco2(t)                        Tax on carbon emissions (EUR per tCO2) additional to a carbon price from carbon market
;

* Set irnw subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set irnwsub     irnwsub(r,t) = 0;
$if      set irnwsub     irnwsub(r,t) = %irnwsub%;
* Set rnw subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set rnwsub      rnwsub(r,t) = 0;
$if      set rnwsub      rnwsub(r,t) = %rnwsub%;
* Set solar PV subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set solsub      solsub(r,t) = 0;
$if      set solsub      solsub(r,t) = %solsub%;
* Set wind subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set windsub     windsub(r,t) = 0;
$if      set windsub     windsub(r,t) = %windsub%;
* Set low-carb subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set nucsub      nucsub(r,t) = 0;
$if      set nucsub      nucsub(r,t) = %lowcarbsub%;
* Set low-carb subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set lowcarbsub  lowcarbsub(r,t) = 0;
$if      set lowcarbsub  lowcarbsub(r,t) = %lowcarbsub%;

* Set irnw subsidy at indicated rate if set
$if not  set irnwsub_cap     irnwsub_cap(r,t) = 0;
$if      set irnwsub_cap     irnwsub_cap(r,t) = %irnwsub_cap%;
* Set rnw subsidy at indicated rate if set
$if not  set rnwsub_cap      rnwsub_cap(r,t) = 0;
$if      set rnwsub_cap      rnwsub_cap(r,t) = %rnwsub_cap%;
* Set solar PV subsidy at indicated rate if set
$if not  set solsub_cap      solsub_cap(r,t) = 0;
$if      set solsub_cap      solsub_cap(r,t) = %solsub_cap%;
* Set wind subsidy at indicated rate if set
$if not  set windsub_cap     windsub_cap(r,t) = 0;
$if      set windsub_cap     windsub_cap(r,t) = %windsub_cap%;
* Set low-carb subsidy at indicated rate if set
$if not  set nucsub_cap      nucsub_cap(r,t) = 0;
$if      set nucsub_cap      nucsub_cap(r,t) = %lowcarbsub_cap%;
* Set low-carb subsidy at indicated rate if set
$if not  set lowcarbsub_cap  lowcarbsub_cap(r,t) = 0;
$if      set lowcarbsub_cap  lowcarbsub_cap(r,t) = %lowcarbsub_cap%;

* Set tax rate on carbon emissions if set
$if not  set taxco2      taxco2(t) = 0;
$if      set taxco2      taxco2(t)$(t.val = 2015) = 0;
$if      set taxco2      taxco2(t)$(t.val > 2015) = %taxco2% * ORD(t);

* Set tax rate at constant growth rate of carbon emissions if set
$if not  set taxco2g     taxco2(t) = 0;
$if      set taxco2g     taxco2(t)$(t.val = 2015) = 0 ;
$if      set taxco2g     taxco2(t)$(t.val = 2020) = (1 + (%taxco2g% * 1e-2)) *  pco2("2015");
$if      set taxco2g     taxco2(t)$(t.val >= 2025) = (1 + (%taxco2g% * 1e-2)) * taxco2(t-1);

* * * Myopic module
set
$if not  set myopic                      tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030,2035,2040,2045,2050/

$if      set myopic2015 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2015/
$if      set myopic2020 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2020/
$if      set myopic2025 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2025/
$if      set myopic2030 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2030/
$if      set myopic2035 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2035/
$if      set myopic2040 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2040/
$if      set myopic2045 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2045/
$if      set myopic2050 $if set overlap1 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2015,2020/
$if      set myopic2020 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2020,2025/
$if      set myopic2025 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2025,2030/
$if      set myopic2030 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2030,2035/
$if      set myopic2035 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2035,2040/
$if      set myopic2040 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2040,2045/
$if      set myopic2045 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap2 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025/
$if      set myopic2020 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030/
$if      set myopic2025 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035/
$if      set myopic2030 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040/
$if      set myopic2035 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045/
$if      set myopic2040 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap3 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030/
$if      set myopic2020 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030,2035/
$if      set myopic2025 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035,2040/
$if      set myopic2030 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040,2045/
$if      set myopic2035 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap4 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030,2035/
$if      set myopic2020 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040/
$if      set myopic2025 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045/
$if      set myopic2030 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045,250/
$if      set myopic2040 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap5 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030,2035,2040/
$if      set myopic2020 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040,2045/
$if      set myopic2025 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045,2050/
$if      set myopic2030 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap6 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030,2035,2040,2045/
$if      set myopic2020 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040,2045,2050/
$if      set myopic2025 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045,2050/
$if      set myopic2030 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap7 tmyopic(t)      Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2015,2020,2025,2030,2035,2040,2045,2050/
$if      set myopic2020 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040,2045,2050/
$if      set myopic2025 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045,2050/
$if      set myopic2030 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if set overlap8 tmyopic(t)      Myopic (or static) optimization year /2050/
;

* * * Define correspoinding parameters for myopic and overlapping myopic runs
parameter
$if not  set myopic                      tmyopicLO         Myopic (or static) optimization year /2015/
$if not  set myopic                      tmyopicUP         Myopic (or static) optimization year /2500/

$if      set myopic2015                  tmyopicLO         Myopic (or static) optimization year /2015/
$if      set myopic2020                  tmyopicLO         Myopic (or static) optimization year /2020/
$if      set myopic2025                  tmyopicLO         Myopic (or static) optimization year /2025/
$if      set myopic2030                  tmyopicLO         Myopic (or static) optimization year /2030/
$if      set myopic2035                  tmyopicLO         Myopic (or static) optimization year /2035/
$if      set myopic2040                  tmyopicLO         Myopic (or static) optimization year /2040/
$if      set myopic2045                  tmyopicLO         Myopic (or static) optimization year /2045/
$if      set myopic2050                  tmyopicLO         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2015/
$if      set myopic2020 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2020/
$if      set myopic2025 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2025/
$if      set myopic2030 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2035 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2040 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2045 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2050 $if set overlap1 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2020/
$if      set myopic2020 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2025/
$if      set myopic2025 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2030 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2035 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2040 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2045 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap2 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2025/
$if      set myopic2020 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2025 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2030 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2035 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2040 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2020 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2025 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2030 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2035 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2020 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2025 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2030 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2020 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2025 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2020 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2025 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2015 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2020 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2025 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
;

parameter
$if not  set myopic      dummy /0/
$if      set myopic2015  dd_myopic_int(r,t)
$if      set myopic2015  bs_myopic_int(s,r,t)
$if      set myopic2015  x_myopic_int(s,i,v,r,t)
$if      set myopic2015  xc_myopic_int(i,v,r,t)
$if      set myopic2015  ix_myopic_int(i,r,t)
$if      set myopic2015  xcs_myopic_int(s,i,v,r,t)
$if      set myopic2015  xtwh_myopic_int(i,v,r,t)
$if      set myopic2015  e_myopic_int(s,transmission,r,r,t)
$if      set myopic2015  it_myopic_int(transmission,r,r,t)
$if      set myopic2015  g_myopic_int(s,j,v,r,t)
$if      set myopic2015  gd_myopic_int(s,j,v,r,t)
$if      set myopic2015  gc_myopic_int(j,v,r,t)
$if      set myopic2015  ig_myopic_int(j,r,t)
$if      set myopic2015  gb_myopic_int(s,j,v,r,t)
$if      set myopic2015  sc_myopic_int(r,t)
$if      set myopic2015  da_myopic_int(r,t)
$if      set myopic2015  tc_myopic_int(transmission,r,r,t)

$if      set myopic      dd_myopic(r,t)
$if      set myopic      bs_myopic(s,r,t)
$if      set myopic      x_myopic(s,i,v,r,t)
$if      set myopic      xc_myopic(i,v,r,t)
$if      set myopic      ix_myopic(i,r,t)
$if      set myopic      xcs_myopic(s,i,v,r,t)
$if      set myopic      xtwh_myopic(i,v,r,t)
$if      set myopic      e_myopic(s,transmission,r,r,t)
$if      set myopic      it_myopic(transmission,r,r,t)
$if      set myopic      g_myopic(s,j,v,r,t)
$if      set myopic      gd_myopic(s,j,v,r,t)
$if      set myopic      gc_myopic(j,v,r,t)
$if      set myopic      ig_myopic(j,r,t)
$if      set myopic      gb_myopic(s,j,v,r,t)
$if      set myopic      sc_myopic(r,t)
$if      set myopic      da_myopic(r,t)
$if      set myopic      tc_myopic(transmission,r,r,t)
;

$if not  set myopic         dummy = 0 ;
$if      set myopic2015     dd_myopic(r,t) = 0 ;
$if      set myopic2015     bs_myopic(s,r,t) = 0 ;
$if      set myopic2015     x_myopic(s,i,v,r,t) = 0 ;
$if      set myopic2015     xc_myopic(i,v,r,t) = 0 ;
$if      set myopic2015     ix_myopic(i,r,t) = 0 ;
$if      set myopic2015     xcs_myopic(s,i,v,r,t) = 0 ;
$if      set myopic2015     xtwh_myopic(i,v,r,t) = 0 ;
$if      set myopic2015     e_myopic(s,transmission,r,rr,t) = 0 ;
$if      set myopic2015     it_myopic(transmission,r,rr,t) = 0 ;
$if      set myopic2015     g_myopic(s,j,v,r,t) = 0 ;
$if      set myopic2015     gd_myopic(s,j,v,r,t) = 0 ;
$if      set myopic2015     gc_myopic(j,v,r,t) = 0 ;
$if      set myopic2015     ig_myopic(j,r,t) = 0 ;
$if      set myopic2015     gb_myopic(s,j,v,r,t) = 0 ;
$if      set myopic2015     sc_myopic(r,t) = 0 ;
$if      set myopic2015     da_myopic(r,t) = 0 ;
$if      set myopic2015     tc_myopic(transmission,r,rr,t) = 0 ;

$if      set myopic $if not set myopic2015      $gdxin limits_%m%\%e%_limits.gdx
$if      set myopic $if not set myopic2015      $load dd_myopic=dd_myopic_int, bs_myopic=bs_myopic_int, x_myopic=x_myopic_int, xc_myopic=xc_myopic_int, ix_myopic=ix_myopic_int, xcs_myopic=xcs_myopic_int, xtwh_myopic=xtwh_myopic_int, e_myopic=e_myopic_int
$if      set myopic $if not set myopic2015      $load it_myopic=it_myopic_int,
$if      set myopic $if not set myopic2015      $load g_myopic=g_myopic_int, gd_myopic=gd_myopic_int, gc_myopic=gc_myopic_int, ig_myopic=ig_myopic_int
$if      set myopic $if not set myopic2015      $load gb_myopic=gb_myopic_int, sc_myopic=sc_myopic_int, da_myopic=da_myopic_int, tc_myopic=tc_myopic_int
$if      set myopic $if not set myopic2015      $gdxin


* * * Sets of relevant generators
set
ivrt(i,v,r,t)           Active vintage-capacity blocks
jvrt(j,v,r,t)           Active storage vintage-capacity blocks
tvrt(transmission,v,t)  Active transmission vintage-capacity blocks
;

* * Generation technologies (ivrt)
* Existing vintages up to their lifetime plus new technologies in future vintage-years are explicitly included
ivrt(i,v,r,t)$(cap(i,v,r) * lifetime(i,v,r,t) or (new(i) and newv(v) and lifetime(i,v,r,t)))     = YES ;
* New-vintage intermittent renewable classes not present in a given region are removed
ivrt(new(i),v,r,t)$(irnw(i) and not sum(xclass(class,i), invlimUP_irnw(r,class)))                = NO ;
* Retrofits and co-fire technologies are removed when underlying block is not present
*ivrt(cofir,v,r,t)$(not sum(xretro(retro,i,vv), ivrt(i,vv,r,t)))                                  = NO ;
*ivrt(retro,v,r,t)$(not sum(xcofir(cofir,i,vv), ivrt(i,vv,r,t)))                                  = NO ;
*ivrt(conve,v,r,t)$(not sum(xconve(conve,i,vv), ivrt(i,vv,r,t)))                                  = NO ;
* New-vintage nuclear in regions with a moratorium are removed
* ivrt("Nuclear",r_nuclim(v,r),t)                                                                  = NO ;
* In certain cases CCS is not permitted, all these technologies are removed
$if      set noccs       ivrt(ccs(i),v,r,t)                                                      = NO ;
* In certain cases trhere is just one "easy" technology (Gas_CCGT)
$if      set easy        ivrt("Bio_CCS",v,r,t)                                                   = NO ;
$if      set easy        ivrt("Bioenergy",v,r,t)                                                 = NO ;
$if      set easy        ivrt("Coal_CCS",v,r,t)                                                  = NO ;
$if      set easy        ivrt("Coal",v,r,t)                                                      = NO ;
$if      set easy        ivrt("Gas_CCS",v,r,t)                                                   = NO ;
$if      set easy        ivrt("Gas_OCGT",v,r,t)                                                  = NO ;
$if      set easy        ivrt("Gas_ST",v,r,t)                                                    = NO ;
$if      set easy        ivrt("Geothermal",v,r,t)                                                = NO ;
$if      set easy        ivrt("Nuclear",v,r,t)                                                   = NO ;
$if      set easy        ivrt("Lignite",v,r,t)                                                   = NO ;

* * Storage technologies (jvrt)
* Existing vintages up to their lifetime plus new technologies in future vintage-years are explicitly included
jvrt(j,v,r,t)$(gcap(j,v,r) * glifetime(j,v,r,t) or (newj(j) and newv(v) and glifetime(j,v,r,t))) = YES ;
* In certain cases storage is not permitted, all storage technologies are removed
$if not  set storage     jvrt(j,v,r,t)                                                           = NO ;

* * Transmission (tvrt)
* Existing vintages up to their lifetime
tvrt(transmission,v,t)$tlifetime(transmission,v,t)                                               = YES ;
* In certain cases transmission is not permitted, all transmission technologies are removed
$if not  set trans       tvrt(transmission,v,t)                                                  = NO ;

* * * Dispatch cost
parameter
discost(i,v,r,t)         Dispatch cost (EUR per MWh el)
;

* * * Define dispatch cost
discost(i,v,r,t)$(ivrt(i,v,r,t) and effrate(i,v,r)) =
*        Variable O&M costs
                           vomcost(i,v,r)
*        Fuel costs (including region-specific price delta)
                         + sum(xfueli(fuel,i), pfuel(fuel,r,t)) / effrate(i,v,r)
*        Corona malus
$if      set corona      * (1 + pricefuel_scale(t,fuel)
*        Regional adder relative
*                         * (1 + pfadd_rel(fuel,r,t)
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if      set co2price    + emit(i,v,r) * co2p(t)
*        CCS costs (from capturing)
$if not  set noccs       + co2captured(i,v,r) * ccscost(r,t)
;


* * * Availability factor matrix (too large to read in)
parameter
af(s,i,r)                Availability factor
;

af(s,i,r) = sum(sm(s,m), af_mon(m,i,r));
* Remove availabilty factor for hydro as they are handled through vrsc in the dynamic model
af(s,"Hydro",r) = 1;
* Remove availability factor for the 4NEMO model comparison
$if      set af1 af(s,i,r) = 1 ;
* Use reliability values instead
$if not  set af1  reliability(i,v,r) = 1 ;
$if not  set af1 greliability(j,v,r) = 1 ;

* * * Run-time switches to consider alterantive scenarios
* Assume no growth in demand
$if      set flatload    dref(r,t) = 1;
* Treat co-firing as a conversion with fixed dispatch proportions
$if      set cofirconve  xcapadj_cr(cofir) = 1;
* Remove all constraints on transmission additions
$if      set tinf        tinvlimUP(transmission,r,rr,t)$tmap(transmission,r,rr) = inf ;
* Assume no new transmission additions are allowed
$if      set tzero       tinvlimUP(transmissionr,rr,t)$tmap(transmission,r,rr) = 0 ;
* Assume no transmission losses
$if      set notrnspen   trnspen(transmission,r,r) = 1 ;
* Set regional/distribution losses to zero for model comparison
$if      set noregloss   loss(r) = 0;
* Add 160 euro per kW to cost of on-shore wind to reflect intra-region transmission (~$200/kW)
$if      set windcost    capcost(i,v,r)$(wind(i) and capcost(i,v,r)) = capcost(i,v,r) + 160 ;
* Make solar PV 10% cheaper
$if      set solcost     capcost(i,v,r)$(sol(i) and capcost(i,v,r)) = 0.9 * capcost(i,v,r) ;
* Assume no cost increase
$if      set flatcost    capcost(i,v,r) = capcost(i,"2020",r);
* Set transmission and storage values to zero when transmission/storage are disabled
$if not  set trans       tcap(transmission,r,rr) = 0;
$if not  set storage     gcap(j,v,r) = 0;

* * * Declare Model
positive variable
* Demand
DD(r,t)                 Demand index relative to reference (only relevant for demand-module)
DA(r,t)                 Annual total demand by region (TWh)
BS(s,r,t)               Lost load (backstop demand option) (GW)
* Generation
X(s,i,v,r,t)            Unit dispatch by segment (GW)
XC(i,v,r,t)             Installed generation capacity (GW)
IX(i,r,t)               New vintage investment (total GW to be added from t-1 to t) (GW)
XCS(s,i,v,r,t)          Copies of XC over s for sparsity purposes (GW)
XTWH(i,v,r,t)           Annual generation for sparsity purposes (TWh)
* Storage
G(s,j,v,r,t)            Energy storage charge (GW)
GD(s,j,v,r,t)           Energy storage discharge (GW)
GC(j,v,r,t)             Energy storage charge-discharge capacity (GW)
IG(j,r,t)               Investment in storage charge-discharge capacity (GW)
GB(s,j,v,r,t)           Energy storage accumulated balance (GWh)
* Transmission
E(s,transmission,r,r,t) Bilateral trade flows by load segment (GW)
IT(transmission,r,r,t)  Investment in transmission (total GW to be added from t-1 to t)
* Market and policy variables
DBS(dbclass,t)          Dedicated biomass supply by class (MWh)
DBSR(dbclass,r,t)       Dedicated biomass supply by class by region (MWh)
NGS(ngclass,t)          Total supply of natural gas by class (MWh)
NGSR(ngclass,r,t)       Total supply of natural gas by class by region (MWh)
CBC(t)                  Cumulative banked credits (GtCO2)
CO2ELEC(t)              CO2 emissions from power generation (GtCO2)
CO2IND(t)               CO2 emissions from other EU ETS sectors (industry) (GtCO2)
* Potential variables
SC(r,t)                 Annual flow of geologically stored carbon (MtCO2)
;

variable
SURPLUS                 Social surplus (negative) (million EUR)
* Transmission
TC(transmission,r,r,t)  New Trade flow capacity (GW)
* Market and policy
NBC(t)                  Net banked credits (GtCO2)
;

equation
objdef                           Objection function -- definition of surplus
*Demand equations
demand(s,r,t)                    Electricity market clearing condition
demand_rsa(s,r,t)                Regional system adequacy condition
demand_ann(r,t)                     Annual total electricity demand
* Generation and capacity
capacity(s,i,v,r,t)              Generation capacity constraint on dispatch
capacity_bio(s,i,v,r,t)          Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_dsp(s,i,v,r,t)          Dispatched capacity
capacity_nsp(s,i,v,r,t)          Non-dispatched capacity
capacity_nucmin(s,i,v,r,t)       Minimum dispatch of nuclear capacity
capacity_techmin(s,i,v,r,t)      Minimum dispatch of other capacity
capacity_cofir(s,i,r,t)          Coal and Lignite capacity can be dispatched with or without co-fire
invest(i,v,r,t)                  Accumulation of annual investment flows
exlife(i,v,r,t)                  Existing capacity including conversions
exlife2015(i,v,r,t)              Existing capacity in 2015 is fix
exlife2020(i,v,r,t)              Existing capacity in 2020 is fix
exlife_bio(i,v,r,t)              Existing capacity of bioenergy is fix to avoid decomm (no subsidy implemented yet)
*MM (todo): Repair retrofit and cofiring constraints
*retro(i,r,t)                     Retrofit eligibility for existing coal
*
newlife(i,v,r,t)                 New vintages are subject to lifetime constraint
retire(i,v,r,t)                  Monotonicity constraint on installed capacity
investlimUP(i,r,t)               Upper limits on investment (region)
investlimLO(i,r,t)               Lower limits on investment (region)
investlimUP_eu(i,t)              Upper limits on investment (EU)
investlimUP_irnw(class,r,t)      Limits on total installed capacity of intermittent renewables
* Storage equations
chargelim(s,j,v,r,t)             Charge cannot exceed capacity
dischargelim(s,j,v,r,t)          Discharge cannot exceed capacity
storagebal(s,j,v,r,t)            Storage balance accumulation
storagelim(s,j,v,r,t)            Storage reservoir capacity
ginvest(j,v,r,t)                 Investment in storage charge-discharge capacity
gexlife(j,v,r,t)                 Existing storage capacity
gexlife2015(j,v,r,t)             Existing capacity in 2015 is fix
gexlife2020(j,v,r,t)             Existing capacity in 2020 is fix
gexlife_pump(j,v,r,t)            Existing capacity of pumphydro is fix to avoid decomm (for the myopic runs)
gnewlife(j,v,r,t)                Storage wew vintages are subject to lifetime constraint
gretire(j,v,r,t)                 Monotonicity constraint on installed storage capacity
ginvestlimUP(j,r,t)              Upper limits on storage investment (region)
ginvestlimLO(j,r,t)              Lower limits on storage investment (region)
ginvestlimUP_eu(j,t)             Upper limits on storage investment (EU)
* Transmission equations
tcapacity(s,transmission,r,r,t)  Transmission capacity constraint on trade
tinvest(transmission,r,r,t)      Accumulation of annual transmission investment flows
tinvestlimUP(transmission,r,r,t) Upper limit on total transmission capacity (between regions)
tinvestlimLO(transmission,r,r,t) Lower limit on total transmission capacity (between regions)
tinvestlimUP_eu(transmission,t)  Upper limit on per period transmission investment (EU)
* Market equations
biomarket(t)                     System-wide market for bioenergy or supply equal demand for bioenergy
biomarket_r(r,t)                 Supply equal demand for bioenergy (regional)
gasmarket(t)                     System-wide market for natural gas or supply equal demand for natural gas
gasmarket_r(r,t)                 Supply equal demand for natural gas (regional)
* EU ETS and MSR
euets_market(t)                  System-wide calibration for co2 emisisons
euets_co2elec(t)                 CO2 emissions from power generation
euets_co2ind(t)                  CO2 emissions from other EU ETS sectors (industry)
euets_itnbc                      Intertemporal budget constraint on net banked credits for system-wide market for co2 emissions
euets_cbcdf(t)                   Definition of cumulative banked credits for system-wide market for co2 emissions
* Policy equations
rnwtgtmarket(t)                  Target market for renewables (system)
rnwtgtmarket_r(r,t)              Target market for renewables (regional)
irnwtgtmarket(t)                 Target market for intermittent renewables (system)
irnwtgtmarket_r(r,t)             Target market for interimittet renewables (regional)
co2market(t)                     Cap market for CO2 emissions (system)
itnbc                            Net banked credits (NBC) for cap market for CO2 emissions (system)
cbcdf(t)                         Cumulative banked credits (CBC) for cap market for CO2 emissions (regional)
co2market_r(r,t)                 Cap market for CO2 emissions (regional)
coalphaseout(t)                  Policy constraint on hard coal phase out
coalphaseout_r(r,t)              Policy constraint on hard coal phase out (regions)
lignphaseout(t)                  Policy constraint on lignite phase out
lignphaseout_r(r,t)              Policy constraint on lignite phase out (regions)
nucphaseout(t)                   Policy constraint on nuclear phase out
nucphaseout_r(r,t)               Policy constraint on nuclear phase out (regions)
* Limit equations
cumbio(t)                        Limits on cumulative use of bioenergy (per region)
cumbio_r(r,t)                    Limits on cumulative use of bioenergy (per region)
ccsflow(t)                       Annual flow of captured carbon for geologic storage
ccsflow_r(r,t)                   Annual flow of captured carbon for geologic storage
cumccs                           Limits on cumulative geologic storage of carbon
cumccs_r(r)                      Limits on cumulative geologic storage of carbon
* Structual equations
xtwhdef(i,v,r,t)                 Calculate XTWH from X
copyxc(s,i,v,r,t)                Make copies of XC in XCS
;


* * * Objective function definition

* Surplus is defined in million EUR
objdef..
*        Negative of
         SURPLUS =e=
*        Sum over all time period t
         !! begin period sum
                 sum(t$tmyopic(t),
*                All costs are subject to the dicount factor dfact(t) which includes already the number of years
*                        Base model: 2015, 2020, ... but 2015 = 2015, 2020 = 2016-2020, ...
*                        Base model: 2015 = 2015-2017, 2020 = 2018-2022
*                        Corona model: 2015 = 2019, 2020 = 2020, 2025 = 2021-2025, ...
*                        Future: 2020 = 2020, 2025 = 2021-2025, 2050, 2075, 2100 ...
*                        Most plausible: 2025 = 2025-2029, ...
                 dfact_model(t) *
*                        Corona        dfact_inv(t) = tailored for 2021 (2025 period)
*                        Corona        dfact_gen(t) = tailored for 2021-2025(whole period)
*                Sum over all regions r
                 !! begin region sum
                 sum(r,

*        INVESTMENT COST
                 !! begin investment cost
*                Investment flows are annualized evenly across period in dynamic model (we need nyrs because investments happens once only but production nyrs-times)
*                        Base model: nyrs("2015") = 1, nyrs("2020") = 5, ...
*                        Corona model: nyrs("2020) = 1, nyrs("2025") = 5, ...
                 1 / nyrs(t) * (1 + tk) * ( 0
*                                                Investment costs follow from capacity cost (EUR/kW) of total period investments (GW)
*                                                MM (todo): check whether or not the myopic model works with the current specification of "annui" (or others)
$if                              set normal    + sharenormal * sum(new,                                       IX(new,r,t)                            * sum(tv(t,v)$ivrt(new,v,r,t),         capcost(new,v,r)           *  endeffect(new,v,r,t) ))
$if      set storage     $if     set normal    + sharenormal * sum(newj,                                      IG(newj,r,t)                           * sum(tv(t,v)$jvrt(newj,v,r,t),       gcapcost(newj,v,r)          * gendeffect(newj,v,r,t) ))
$if      set trans       $if     set normal    + sharenormal * sum((rr,transmission)$tmap(transmission,r,rr), IT(transmission,r,rr,t)                * sum(tv(t,v)$tvrt(transmission,v,t), tcapcost(transmission,r,rr) * tendeffect(transmission,v,t) ))

*                                                Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
$if                              set annui     + shareannui  * sum(new,                                       annuity_model(new)           * nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX(new,r,tt)            *  capcost(new,v,r)           * deprtime(new,v,r,tt) ) )
$if      set storage     $if     set annui     + shareannui  * sum(newj,                                                                     nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IG(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt) * gannuity_model(newj,v) ) )
$if      set trans       $if     set annui     + shareannui  * sum((rr,transmission)$tmap(transmission,r,rr), tannuity_model(transmission) * nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(transmission,v,tt)), IT(transmission,r,rr,t) * tcapcost(transmission,r,rr) * tdeprtime(transmission,v,tt) ) )

*                                                Investment costs follow from WACC (%) of capital stock (EUR/kW * GW)
$if                              set ccost     + shareccost  * sum(new,                                        irate_model(new)            * nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX(new,r,tt)            * capcost(new,v,r)            * deprtime(new,v,r,tt) ) )
$if      set storage     $if     set ccost     + shareccost  * sum(newj,                                      girate_model(newj)           * nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IG(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt) ) )
$if      set trans       $if     set ccost     + shareccost  * sum((rr,transmission)$tmap(transmission,r,rr), tirate_model(transmission)   * nyrs(t) * sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(transmission,v,tt)), IT(transmission,r,rr,t) * tcapcost(transmission,r,rr) * tdeprtime(transmission,v,tt) ) )
                 )
                 !! end investment cost

*        R&D COST
*                MM (todo): Implement R&D cost here
                 !! begin R&D cost
                 !! end R&D cost

*        DISPATCH COST
                 !! begin dispatch cost
*                        Dispatch cost (EUR/MWh) for generation (GWh)
*                        MM (todo): Include the natural and dedicated biomass market here (see old model version for the way of doing so)
                         + 1e-3 * sum(ivrt(i,v,r,t),                               discost(i,v,r,t)               * sum(s,  X(s,i,v,r,t)                  * hours(s) ))
*                        Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if      set storage     + 1e-3 * sum(jvrt(j,v,r,t),                              gvomcost(j,v,r)                 * sum(s, (G(s,j,v,r,t) + GD(s,j,v,r,t)) * hours(s) ))
*                        Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if      set trans       + 1e-3 * sum((transmission,rr)$tmap(transmission,r,rr), tvomcost(transmission,r,rr)      * sum(s, E(s,transmission,r,rr,t) * hours(s) ))
*                        Cost of biomass fuel supply
*  todo (mm): check pf_r naming
$if      set biomark_r   + sum(dbclass, DBSR(dbclass,r,t) * (dbcost_r(dbclass,r,t) - pfuel("Bioenergy",r,t)) )
$if      set gasmark_r   + sum(ngclass, NGSR(ngclass,r,t) * (ngcost_r(ngclass,r,t) - pfuel("Gas",r,t)) )
                 !! end dispatch cost


                 !! end dispatch cost

*        POLICY COST are from the perspective of the investor/firm/generator whose costs decrease when receiving a subsidy but increase by taxing (this is not a welfare optimum)
                 !! begin policy cost
*                        Production subsidy (also for old vintage capacity in the moment, one can play around with newv(v))
*                        MM (todo): Think about introducing subsidy by vintage level to reflect feed-in tariff structures
                         - 1e-3 * sum(ivrt(i,v,r,t)$irnw(i),             irnwsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
                         - 1e-3 * sum(ivrt(i,v,r,t)$rnw(i),              rnwsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
                         - 1e-3 * sum(ivrt(i,v,r,t)$sol(i),              solsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
                         - 1e-3 * sum(ivrt(i,v,r,t)$wind(i),             windsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
                         - 1e-3 * sum(ivrt(i,v,r,t)$nuc(i),              nucsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
                         - 1e-3 * sum(ivrt(i,v,r,t)$lowcarb(i),          lowcarbsub(r,t)         * sum(s, X(s,i,v,r,t) * hours(s) ))
*                        Capacity subsidy (paid for new vintage capacity only)
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$irnw(i),       irnwsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$rnw(i),        rnwsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$sol(i),        solsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$wind(i),       windsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$nuc(i),        nucsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
                         - 1e-3 * sum(ivrt(i,newv(v),r,t)$lowcarb(i),    lowcarbsub_cap(r,t)     * sum(tv(t,v), IX(i,r,t) ))
*                        CO2 emission tax
                         + 1e-3 * sum(ivrt(i,v,r,t)$emit(i,v,r),         emit(i,v,r) * taxco2(t) * sum(s, X(s,i,v,r,t) * hours(s) ))
                 !! end policy cost

*        SOCIAL COST should be considered here when "internalized by a tax" or the system gives a social planner perspective (when not considered by investors then do not include here but calculate back-of-the-envelope in rpt)
                 !! begin social cost
*                        Cost (EUR/MWh) of lost load/backstop (GWh)
                         + 1e-3 * voll(r,t) * sum(s, BS(s,r,t) * hours(s))
*                        Public acceptance cost (EUR/MWh) for incremental nuclear generation (GWh)
*                        MM (todo): Think about public acceptance cost for nuclear capacity (and also other capacities)
$if      set scn         + 1e-3 * sum(ivrt(i,v,r,t), scni(i,v,r,t))     * sum(s, X(s,i,v,r,t) * hours(s) ))
*                        Social cost of air pollution (EUR per MWh) from generation (GWh)
$if      set scap        + 1e-3 * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(s, X(s,i,v,r,t) * hours(s) ))
*                        Social cost of carbon (EUR per MWh) from generation (GWh)
$if      set scc         + 1e-3 * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(s, X(s,i,v,r,t) * hours(s) ))
*                        MM (todo): Introduce wind turbine public cost from visibility and noise (metric was already implemented once from Christoph) here (wait for the calibration from the Master thesis of Patrick)
                 !! end social cost

*        FIXED COST
                 !! begin fixed cost
*                        Fixed operation and maintenance cost (EUR/kW) for generation capacity (GW)
                         + sum(ivrt(i,v,r,t),                                    XC(i,v,r,t)                                         *  fomcost(i,v,r)            )
*                        Fixed operation and maintenance cost (EUR/kW) for storage capacity (GW)
$if      set storage     + sum(jvrt(j,v,r,t),                                    GC(j,v,r,t)                                         * gfomcost(j,v,r)            )
*                        Fixed operation and maintenance cost (EUR/kW) for transmission capacity (GW)
$if      set trans       + sum((transmission,rr)$tmap(transmission,r,rr),       (TC(transmission,r,rr,t) + tcap(transmission,r,rr)) * tfomcost(transmission,r,rr) )
                 !! end fixed cost
                 )
                 !! end region sum
$if      set biomark     + sum(dbclass, DBS(dbclass,t) * (dbcost(dbclass,t) - sum((r,dbclass), pfuel("Bioenergy",r,t) * dblim_r(dbclass,r,t))) )
$if      set gasmark     + sum(ngclass, NGS(ngclass,t) * (ngcost(ngclass,t) - sum((r,ngclass), pfuel("Gas",r,t) * nglim_r(ngclass,r,t))) )
         )
         !! end time period sum
;



* * * * * Demand equations
* * * Electricity market clearance condition (in each segment)
demand(s,r,t)$tmyopic(t)..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in EUR/MWh)
                         1e-3 * hours(s)
*        Dispatched generation in region
                         * (sum(ivrt(i,v,r,t), X(s,i,v,r,t))
*        Plus inter-region imports
$if      set trans       + sum((transmission,rr)$tmap(transmission,rr,r), E(s,transmission,rr,r,t))
*        Less inter-region exports (penalty for transmission losses is charged on the export site only)
* MM(todo): Adjust in other versions
$if      set trans       - sum((transmission,rr)$tmap(transmission,r,rr), E(s,transmission,r,rr,t) / trnspen(transmission,r,rr))
*        Plus discharges from storage times discharge efficiency (less supply than stored) less charges from storage (the penalties apply at the storage accumulation)
$if      set storage     + sum(jvrt(j,v,r,t), GD(s,j,v,r,t) * dchrgpen(j,v,r) - G(s,j,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set lostload    + BS(s,r,t) * (1 + loss(r))
         )
*        Equals (annually scaled) demand including losses
         =e=             1e-3 * hours(s) * DD(r,t) * dref(r,t) * load(s,r) * (1 + loss(r))
$if      set corona      * (1 + demandelec_scale(t))
;

* * * Regional system adequacy constraint
* Hypothetical shadow electricity market with no transmission in the default version
* In the 4NEMO version it has changed by using capcredits. Here, transmission capacity is offered a tcapcredit of 0.1
demand_rsa(peak(s,r),t)$(not tbase(t) and tmyopic(t))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in euro per MWh)
                         1e-3 * hours(s) * (
*        Upper bound on available generation in region
                         + sum(ivrt(i,v,r,t), XCS(s,i,v,r,t) *  capcred(i,v,r))
*        Plus discharges from storage less charges (plus penalty)
$if      set storage     + sum(jvrt(j,v,r,t),  GD(s,j,v,r,t) * gcapcred(j,v,r))
*        Plus inter-region imports
$if      set trans       + sum((transmission,rr)$tmap(transmission,rr,r), TC(transmission,rr,r,t) * tcapcred(transmission,rr,r))
         )
*        Equals (annually scaled) demand including losses
         =g=             1e-3 * hours(s) * DD(r,t) * dref(r,t) * load(s,r) * (1 + loss(r))
$if      set corona      * (1 + demandelec_scale(t))
;

* * * Annual demand is sum of segment loads
demand_ann(r,t)$tmyopic(t)..
         DA(r,t) =e= 1e-3 * DD(r,t) * dref(r,t) * sum(s,  load(s,r) * hours(s) )
$if      set corona      * (1 + demandelec_scale(t))
;

* * * * * Generation and capacity equations

* * * Dispatch of units cannot exceed available capacity
* A variety of adjustment factors are applied to capacity to determine potential dispatch.
* These include availability factors that may or may not vary by segment (af and af_i),
* variable resource factors that vary by segment (vrsc), and in some cases time trends in the
* shape of availability or variability (af_t and vrsc_t).  In each case, the parameter applies
* to only a subset of technologies.
* To avoid creating a very large parameter matrix with many placeholder entries of 1, we use the following construct to perform a conditional product:
* 1 + (par(i) - 1)$par(i) = par(i) if it is defined, 1 otherwise
* This constuct requires that Eps is used for parameters where only some of the segments are populated.
* In these cases the Eps / zero / missing value should be treated as a zero instead of an implicit 1.
* The model will abort if vrsc is equal to 0 (rather than Eps) for an allowable renewable class.

* af are the monthly availability factor of dispatchable power and vrsc those of intermittent renewables (reliability is used if not af)
capacity(s,ivrt(i,v,r,t))$tmyopic(t)..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Sometime we differentiate between technologies that are dispatchable (dspt) and those that are not (ndsp) (* This constraints is deactivated in the model setup when must-run is not "yes")
capacity_dsp(s,ivrt(dspt(i),v,r,t))$tmyopic(t)..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r))* (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Non-dispatchable technologies cannot adjust their production (see equality sign) (This constraints is deactivated in the model setup when must-run is not "yes")
capacity_nsp(s,ivrt(ndsp(i),v,r,t))$tmyopic(t)..
                 X(s,i,v,r,t) =e=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r))* (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Bioenergy is must-run (This constraint is also generally deactivated (could come with numerical difficulties but need further testing))
capacity_bio(s,ivrt(i,v,r,t))$(sameas(i,"Bioenergy") and tmyopic(t))..
                 X(s,i,v,r,t) =e=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r))* (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Minimum dispatch of nuclear (this condition is deactivitated if not "nucmin=yes")
capacity_nucmin(s,i,v,r,t)$(ivrt("Nuclear",v,r,t) and tmyopic(t))..
                 X(s,"Nuclear",v,r,t) =g=  XCS(s,"Nuclear",v,r,t) * mindisp("Nuclear",v,r) * (1 + (reliability("Nuclear",v,r)-1)$reliability("Nuclear",v,r)) * (1 + (af(s,"Nuclear",r)-1)$af(s,"Nuclear",r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Minimum dispatch of all technologies (this condition is deactivitated if not "techmin=yes"
capacity_techmin(s,ivrt(i,v,r,t))$(mindisp(i,v,r) and tmyopic(t))..
                 X(s,i,v,r,t) =g=  XCS(s,i,v,r,t) * mindisp(i,v,r) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ;

* Co-firing dispatch has more complicated constraints (Existing coal or lignite capacity can be dispatched with or without co-firing) (co-fire is deactivated if not "cofiring=yes")
*capacity_cofir(s,ivrt(cofir,v,r,t))$tmyopic(t)..
*        Generation in co-fire mode (from any vintage) plus all-lign/coal generation in the underlying capacity blocks
*         sum(ivrt(cofir,v,r,t), X(s,cofir,v,r,t)) + sum(xcofir(cofir,i,vv)$ivrt(i,vv,r,t), X(s,i,vv,r,t))
*        Must not exceed availability of the underlying capacity blocks
*         =l= sum(xcofir(cofir,i,vv)$ivrt(i,vv,r,t), XCS(s,i,vv,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,r)-1)$af(s,i,r)) * (1 + (vrsc(s,i,r)-1)$vrsc(s,i,r)) ) ;


* * * Investment flows accumulate as new vintage capacity
invest(new(i),newv(v),r,t)$(tv(t,v) and tmyopic(t))..
         XC(i,v,r,t) =l= IX(i,r,t) ;

* * * Existing vintages have fixed lifetime
* Cannot be decommissioned in 2015
exlife2015(ivrt(i,oldv(v),r,t))$(sameas(t,"2015") and tmyopic(t)) ..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;
* Cannot be decommissioned in 2020 (only active in Corona-Paper but maybe useful for intermediate-calibration)
exlife2020(ivrt(i,oldv(v),r,t))$(sameas(t,"2020") and tmyopic(t)) ..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;
* Bioenergy cannot be decommissioned (only active in Corona-Paper but makes sense when considering bioenergy subsidies)
exlife_bio(ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy") and not sameas(t,"2015") and tmyopic(t)
$if      set corona      and not sameas(t,"2020")
         ) ..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;
* Can be decommissioned in >2015
exlife(ivrt(i,oldv(v),r,t))$(not sameas(t,"2015") and tmyopic(t)
$if      set corona      and not sameas(t,"2020")
         ) ..
         XC(i,v,r,t)
* subject to conversion or retrofit for existing coal and lignite capacity
$if      set retrofit    + sum((xretro(retrp,i,v),vv), XC(cr,vv,r,t) * xcapadj_retro(retro))
         =l= cap(i,v,r) * lifetime(i,v,r,t) ;

* * * Upper bounds on lignite and coal CCS retrofit
* Retro is deactivatd if no "retrofit=yes"
*retro(retro,r,t)$tmyopic(t)..
*         sum(ivrt(retro,v,r,t), XC(retro,v,r,t) * xcapadj_retro(retro)) =l= ccs_retro(retro,r);

* * * New vintages have a lifetime profile for enforced retirement
newlife(ivrt(new(i),newv(v),r,t))$(not sameas(v,"2050") and tmyopic(t))..
        XC(i,v,r,t) =l= lifetime(i,v,r,t) * sum(tv(tt,v), IX(i,r,tt)) ;

* * * All vintages must be monotonically decreasing (except 2050)
* For myopic runs the tstatic not has to be here because otherwise step-in-step-out of capacities is possible
retire(ivrt(i,v,r,t))$(not sameas(v,"2050") and tmyopic(t))..
        XC(i,v,r,t+1) =l= XC(i,v,r,t) ;

* * * Upper and lower limits on investments based on current pipeline or other regional constraints
* Upper limit
investlimUP(i,r,t)$(new(i) and invlimUP(i,r,t) and tmyopic(t))..
         IX(i,r,t) =l= invlimUP(i,r,t)
* To avoid numerical difficulties when setting the variables (actually, we could try do it as in the myopic runs)
$if      set corona_flim + 0.001
;
* Lower limit
investlimLO(i,r,t)$(new(i) and invlimLO(i,r,t) and tmyopic(t))..
         IX(i,r,t) =g= invlimLO(i,r,t) ;
* Upper limit for whole system (in general deactivated)
investlimUP_eu(i,t)$(invlimUP_eu(i,t) < inf and tmyopic(t))..
         sum(r, IX(i,r,t)) =l= invlimUP_eu(i,t) ;


* * * Upper bound on installed wind (on- and off-shore) and solar capacity (including remaining base year) in each quality class
investlimUP_irnw(class,r,t)$(invlimUP_irnw(r,class) and not sameas(t,"2015") and tmyopic(t)
* To avoid 2020 decommissioning of existing wind and solar (beyond quality classes)
         and not sameas(t,"2020")
         )..
         sum(xclass(class,i)$(idef(i,"wind") or idef(i,"wind-os") or idef(i,"slpv")),
         sum(ivrt(i,v,r,t)$(new(i) and newv(v)), XC(i,v,r,t)) + sum((idef(i,type), iidef(exi,type),oldv), XC(exi,oldv,r,t)) )
         =l= invlimUP_irnw(r,class) ;



* * * * * Transmission equations

* * * Enforce capacity constraint on inter-region trade flows
tcapacity(s,transmission,r,rr,t)$(tmap(transmission,r,rr) and tmyopic(t))..
         E(s,transmission,r,rr,t) =l= tcap(transmission,r,rr) + TC(transmission,r,rr,t) ;

* * * Allow accumulation of transmission capacity investments
* For now, we allow for line decommissioning (happens due to the calibration)
tinvest(transmission,r,rr,t)$(tmap(transmission,r,rr) and tmyopic(t))..
         TC(transmission,r,rr,t) =l= IT(transmission,r,rr,t) + TC(transmission,r,rr,t-1) ;

* * * Upper and lower limits on investments based on current pipeline or other regional constraints
* Upper limit
tinvestlimUP(transmission,r,rr,t)$tmyopic(t)..
         tcap(transmission,r,rr) + TC(transmission,r,rr,t) =l= tinvlimUP(transmission,r,rr,t)
* To avoid numerical difficulties when setting the variables (actually, we could try do it as in the myopic runs)
$if      set corona_flim + 0.001
;
* Lower limit
tinvestlimLO(transmission,r,rr,t)$tmyopic(t)..
         tcap(transmission,r,rr) + TC(transmission,r,rr,t) =g= tinvlimLO(transmission,r,rr,t) ;
* Upper limit for whole system (in general deactivated)
tinvestlimUP_eu(transmission,t)$(tinvlimUP_eu(transmission,t) < inf and tmyopic(t))..
         sum((r,rr), tcap(transmission,r,rr) + TC(transmission,r,rr,t) ) =l= tinvlimUP_eu(transmission,t) ;


* * * * * Storage equations

* * * Storage charge-discharge and accumulation
* Charge must not exceed charge capacity (size of door - entry)
chargelim(s,j,v,r,t)$tmyopic(t)..
         G(s,j,v,r,t)  =l= GC(j,v,r,t) ;
* Discharge must not exceed charge capacity (size of door - exit)
dischargelim(s,j,v,r,t)$tmyopic(t)..
         GD(s,j,v,r,t) =l= GC(j,v,r,t) ;
* Dynamic accumulation of storage balance (automatic discharge and charge efficiency apply here)
storagebal(s,j,v,r,t)$tmyopic(t)..
         GB(s,j,v,r,t) =e= GB(s-1,j,v,r,t) * (1 - dischrg(j,v,r)) +
$if      set storage_absweights   hours(s) *
* MM (comment): number = 100 means that we "implicitly" model 87.6 storage cycles
$if      set storage_relweights   hours(s) * number / 8760 *
         (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) ;
* Accumulated balance must not exceed storage capacity (size of room - reservoir)
storagelim(s,j,v,r,t)$tmyopic(t)..
         GB(s,j,v,r,t) =l= ghours(j,v,r) * GC(j,v,r,t) ;

* * * Allow accumulation of storage charge capacity investments
ginvest(newj(j),newv(v),r,t)$(tv(t,v) and tmyopic(t))..
         GC(j,v,r,t) =l= IG(j,r,t) + GC(j,v,r,t-1)$(sameas(v,"2050") and t.val > 2050);

* * * Existing storage vintages have fixed lifetime
* MM (todo): Think about implementing different constraints for myopic runs since it might be that endogenous decommissioning needs to get disablted anyway
* No decomissioning in 2015
gexlife2015(jvrt(j,oldv(v),r,t))$(sameas(t,"2015") and tmyopic(t))..
         GC(j,v,r,t) =e= gcap(j,v,r) * glifetime(j,v,r,t) ;
* Decommissioning possible >2015
gexlife(jvrt(j,oldv(v),r,t))$(not sameas(t,"2015") and tmyopic(t))..
         GC(j,v,r,t) =l= gcap(j,v,r) * glifetime(j,v,r,t) ;
* Avoid decommissioning of pump storage capacty
gexlife_pump(jvrt(j,oldv(v),r,t))$(sameas(j,"PumpStorage_2015") and not sameas(t,"2015") and tmyopic(t))..
         GC(j,v,r,t) =e= gcap(j,v,r) * glifetime(j,v,r,t) ;

* * * New storage vintages have a lifetime profile for enforced retirement
gnewlife(jvrt(newj(j),newv(v),r,t))$(not sameas(v,"2050") and tmyopic(t))..
        GC(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IG(j,r,tt));

* * * All storage vintages must be monotonically decreasing (except 2050)
gretire(j,v,r,t)$(not sameas(v,"2050") and tmyopic(t))..
        GC(j,v,r,t+1) =l= GC(j,v,r,t) ;

* * * Upper and lower limits
* Upper limit
ginvestlimUP(j,r,t)$tmyopic(t)..
         IG(j,r,t) =l= ginvlimUP(j,r,t)
$if      set corona_flim + 0.001
;
* Lower limit
ginvestlimLO(j,r,t)$tmyopic(t)..
         IG(j,r,t) =g= ginvlimLO(j,r,t) ;
* Upper limits for whole system (again mostly inactive)
ginvestlimUP_eu(j,t)$(newj(j) and ginvlimUP_eu(j,t) < inf)..
         sum(r, IG(j,r,t)) =l= ginvlimUP_eu(j,t) ;

* * * Bioenergy market
* for whole system (allows for trade and the marginal is then the "price")
biomarket(t)$(sum(dbclass, dblim(dbclass,t)) and tmyopic(t))..
               sum(dbclass, DBS(dbclass,t))     =e= sum(r, sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ) ;
* for each region (does not allow for system-wide trade)
biomarket_r(r,t)$(sum(dbclass, dblim_r(dbclass,r,t)) and tmyopic(t))..
               sum(dbclass, DBSR(dbclass,r,t))  =e=        sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ;

* * * Natural gas market
* for whole system (allows for trade and the marginal is then the "price")
gasmarket(t)$(sum(ngclass, nglim(ngclass,t)) and tmyopic(t))..
               sum(ngclass, NGS(ngclass,t))     =e= sum(r, sum(ivrt(i,v,r,t)$(sameas(i,"Gas_OCGT") or sameas(i,"Gas_CCGT") or sameas(i,"Gas_ST") or sameas(i,"Gas_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ) ;
* for each region (does not allow for system-wide trade)
gasmarket_r(r,t)$(sum(ngclass, nglim_r(ngclass,r,t)) and tmyopic(t))..
               sum(ngclass, NGSR(ngclass,r,t))  =e=        sum(ivrt(i,v,r,t)$(sameas(i,"Gas_OCGT") or sameas(i,"Gas_CCGT") or sameas(i,"Gas_ST") or sameas(i,"Gas_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ;

* * * Renewable energy share market
* for whole system (allows for trade and the marginal is then the "price")
rnwtgtmarket(t)$(rnwtgt(t) and tmyopic(t))..
        sum(r, sum(ivrt(rnw(i),v,r,t), XTWH(i,v,r,t)))  =g=  rnwtgt(t)     * sum(r, sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ) ;
* for each region (does not allow for system-wide trade)
rnwtgtmarket_r(r,t)$(rnwtgt_r(r,t) and tmyopic(t))..
               sum(ivrt(rnw(i),v,r,t), XTWH(i,v,r,t))   =g=  rnwtgt_r(r,t) *        sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ;

* * * Intermittent renewable energy share market
* for whole system (allows for trade and the marginal is then the "price")
irnwtgtmarket(t)$(irnwtgt(t) and tmyopic(t))..
        sum(r, sum(ivrt(irnw(i),v,r,t), XTWH(i,v,r,t))) =g= irnwtgt(t)     * sum(r, sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ) ;
* for each region (does not allow for system-wide trade)
irnwtgtmarket_r(r,t)$(irnwtgt_r(r,t) and tmyopic(t))..
               sum(ivrt(irnw(i),v,r,t), XTWH(i,v,r,t))  =g= irnwtgt_r(r,t) *        sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ;

* * * Carbon market
* for whole system (allows for trade and the marginal is then the "price") with potential banking
co2market(t)$(co2cap(t) and (co2cap(t) < inf) and tmyopic(t))..
        co2cap(t) - NBC(t)/nyrs(t) =g= sum(r, sum(ivrt(i,v,r,t)$emit(i,v,r), emit(i,v,r) * XTWH(i,v,r,t)) ) ;
* Cumulative banked credits - when this is constrained to be positive, borrowing is not allowed
cbcdf(t)..
         CBC(t) =e= sum(tt$(tt.val le t.val), NBC(tt)) ;
* Intertemporal bank must balance by 2050
itnbc..
         sum(t, -NBC(t)) =e= 0 ;
* for each region (does not allow for system-wide trade)
co2market_r(r,t)$(co2cap_r(r,t) and (co2cap_r(r,t) < inf) and tmyopic(t))..
        co2cap_r(r,t)              =g=        sum(ivrt(i,v,r,t)$emit(i,v,r), emit(i,v,r) * XTWH(i,v,r,t)) ;

* * * EU ETS market
*MM (todo): handle scope of EU ETS with/out United Kingdom andf Switzerland
euets_market(t)$(euets_supply(t) and (euets_supply(t) < inf) and tmyopic(t))..
$if      set corona_sce0 (euets_supply(t) - NBC(t)/nyrs(t) ) / (1 + co2ind_scale(t))  =g= sum(r, sum(ivrt(i,v,r,t)$emit(i,v,r), emit(i,v,r) * XTWH(i,v,r,t) )) ;
$if not  set corona_sce0 (euets_supply(t) - NBC(t)/nyrs(t) ) - co2ind_abs(t)          =g= sum(r, sum(ivrt(i,v,r,t)$emit(i,v,r), emit(i,v,r) * XTWH(i,v,r,t) )) ;
* Calculate CO2ELEC for reporting
euets_co2elec(t)$tmyopic(t)..
         CO2ELEC(t) =e= sum(r, sum(ivrt(i,v,r,t)$emit(i,v,r), emit(i,v,r) * XTWH(i,v,r,t) )) ;
* Calculate CO2IND for reporting
euets_co2ind(t)$tmyopic(t)..
$if      set corona_sce0 CO2IND(t) =e= co2ind_scale(t) * CO2ELEC(t);
$if not  set corona_sce0 CO2IND(t) =e= co2ind_abs(t) ;
* Cumulative banked credits - when this is constrained to be positive, borrowing is not allowed
euets_cbcdf(t)..
         CBC(t) =e= 1654.91 + sum(tt$(tt.val le t.val), NBC(tt)) ;
* Intertemporal bank must balance by 2050
euets_itnbc..
         sum(t, -NBC(t)) =e= 1654.91 ;

* * * Bioenergy potential
* for whole system (allows for trade and the marginal is then the "price")
cumbio(t)$tmyopic(t)..
         biolim_eu(t) =g= sum(r, sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ) ;
* for each region (does not allow for system-wide trade)
cumbio_r(r,t)$tmyopic(t)..
         biolim(r,t)  =g=        sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS")), (1 / effrate(i,v,r)) * XTWH(i,v,r,t)) * 1e+6 ;

* * * Geologic storage of carbon
* for whole system (system-wide constraints allow for trade and the marginal is then the "price")
ccsflow(t)$(not tbase(t) and tmyopic(t))..       sum(r, SC(r,t)) =e=  sum(r, 1e-3 * sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t)));
cumccs..                                         sum((r,t), nyrs(t) * SC(r,t)) =l= sclim_eu ;
* for each region (does not allow for system-wide trade)
ccsflow_r(r,t)$(not tbase(t) and tmyopic(t))..   SC(r,t)         =e=         1e-3 * sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t));
cumccs_r(r)..                                    sum(t,     nyrs(t) * SC(r,t)) =l= sclim(r) ;

* * * Structural equations to aid solver
xtwhdef(ivrt(i,v,r,t))$tmyopic(t)..         XTWH(i,v,r,t) =e= 1e-3 * sum(s, X(s,i,v,r,t) * hours(s));
copyxc(s,ivrt(i,v,r,t))$tmyopic(t)..        XCS(s,i,v,r,t) =e= XC(i,v,r,t)$(ord(s) eq 1) + XCS(s-1,i,v,r,t)$(ord(s) > 1);

* * * * * Additional constraints
* Turning on/off features
* Sensitivities
* Tweeking the model in the right direction
* .LO .UP .FX are called label

* If price elasticity is 0, fix load:
DD.FX(r,t)$(pelas(r,t) eq 0) = 1;

* No investment in base year
IX.FX(i,r,tbase) = 0 ;
$if      set trans       IT.FX(transmission,r,rr,tbase) = 0 ;
$if      set storage     IG.FX(j,r,tbase) = 0 ;

* No transmission when not set transmission
$if not  set trans       IT.FX(transmission,r,rr,t) = 0 ;
$if not  set trans       E.FX(s,transmission,r,rr,t) = 0 ;
$if not  set trans       TC.FX(transmission,r,rr,t) = 0 ;

* No storage when not set storage
$if not  set storage     IG.FX(j,r,t) = 0 ;
$if not  set storage     G.FX(s,j,v,r,t) = 0 ;
$if not  set storage     GB.FX(s,j,v,r,t) = 0 ;
$if not  set storage     GC.FX(j,v,r,t) = 0 ;
$if not  set storage     GD.FX(s,j,v,r,t) = 0 ;

* MM (todo): Initial storage level should be empty


* Remove backstop unless explicitly allowed
$if not  set lostload    BS.FX(s,r,t) = 0 ;

* Restrict banking if specified
$if not  set banking     NBC.FX(t) = 0 ;
$if not  set banking     CBC.FX(t) = 0 ;

* In certain cases, CCS is unavailable (todo: try doing this by removing from ivrt set)
$if      set noccs       IX.FX(ccs(i),r,t) = 0;

* No new nuclear capacity allowed
$if      set nonuc       IX.FX("nuclear",r,t) = 0;

* Variation run additions for the "easy scenario"
$if      set easy        IX.FX(ccs(i),r,t) = 0;
$if      set easy        IX.FX("Bio_CCS",r,t) = 0;
$if      set easy        IX.FX("Bioenergy",r,t) = 0;
$if      set easy        IX.FX("Coal",r,t) = 0;
$if      set easy        IX.FX("Coal_CCS",r,t) = 0;
$if      set easy        IX.FX("Gas_CCS",r,t) = 0;
$if      set easy        IX.FX("Gas_OCGT",r,t) = 0;
$if      set easy        IX.FX("Gas_ST",r,t) = 0;
$if      set easy        IX.FX("Geothermal",r,t) = 0;
$if      set easy        IX.FX("Nuclear",r,t) = 0;
$if      set easy        IX.FX("Lignite",r,t) = 0;

* EU ETS calibration year fixes
$if      set euets       NBC.fx("2015") = nbc_start("2015") ;
$if      set euets       CO2ELEC.up("2015") = co2elec_up("2015") ;
$if      set euets       CO2ELEC.lo("2015") = co2elec_low("2015") ;
$if      set euets       CO2ELEC.up(t) = co2elec_up(t) ;
$if      set euets       CO2ELEC.lo(t) = co2elec_low(t) ;

* Myopic model fixes
$if      set myopic      DD.FX(r,t)$(t.val < tmyopicLO)                   = dd_myopic(r,t) ;
$if      set myopic      BS.FX(s,r,t)$(t.val < tmyopicLO)                 = bs_myopic(s,r,t) ;
$if      set myopic      X.FX(s,i,v,r,t)$(t.val < tmyopicLO)              = x_myopic(s,i,v,r,t) ;
$if      set myopic      XC.FX(i,v,r,t)$(t.val < tmyopicLO)               = xc_myopic(i,v,r,t) ;
$if      set myopic      IX.FX(i,r,t)$(t.val < tmyopicLO)                 = ix_myopic(i,r,t) ;
$if      set myopic      XCS.FX(s,i,v,r,t)$(t.val < tmyopicLO)            = xcs_myopic(s,i,v,r,t) ;
$if      set myopic      XTWH.FX(i,v,r,t)$(t.val < tmyopicLO)             = xtwh_myopic(i,v,r,t) ;
$if      set myopic      E.FX(s,transmission,r,rr,t)$(t.val < tmyopicLO)  = e_myopic(s,transmission,r,rr,t) ;
$if      set myopic      IT.FX(transmission,r,rr,t)$(t.val < tmyopicLO)   = it_myopic(transmission,r,rr,t) ;
$if      set myopic      G.FX(s,j,v,r,t)$(t.val < tmyopicLO)              = g_myopic(s,j,v,r,t) ;
$if      set myopic      GD.FX(s,j,v,r,t)$(t.val < tmyopicLO)             = gd_myopic(s,j,v,r,t) ;
$if      set myopic      GC.FX(j,v,r,t)$(t.val < tmyopicLO)               = gc_myopic(j,v,r,t) ;
$if      set myopic      IG.FX(j,r,t)$(t.val < tmyopicLO)                 = ig_myopic(j,r,t) ;
$if      set myopic      GB.FX(s,j,v,r,t)$(t.val < tmyopicLO)             = gb_myopic(s,j,v,r,t) ;
$if      set myopic      SC.FX(r,t)$(t.val < tmyopicLO)                   = sc_myopic(r,t) ;
$if      set myopic      DA.FX(r,t)$(t.val < tmyopicLO)                   = da_myopic(r,t) ;
$if      set myopic      TC.FX(transmission,r,rr,t)$(t.val < tmyopicLO)   = tc_myopic(transmission,r,rr,t) ;

$if      set myopic      DD.FX(r,t)$(t.val > tmyopicUP)                   = eps ;
$if      set myopic      BS.FX(s,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      X.FX(s,i,v,r,t)$(t.val > tmyopicUP)              = eps ;
$if      set myopic      XC.FX(i,v,r,t)$(t.val > tmyopicUP)               = eps ;
$if      set myopic      IX.FX(i,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      XCS.FX(s,i,v,r,t)$(t.val > tmyopicUP)            = eps ;
$if      set myopic      XTWH.FX(i,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      E.FX(s,transmission,r,rr,t)$(t.val > tmyopicUP)  = eps ;
$if      set myopic      IT.FX(transmission,r,rr,t)$(t.val > tmyopicUP)   = eps ;
$if      set myopic      G.FX(s,j,v,r,t)$(t.val > tmyopicUP)              = eps ;
$if      set myopic      GD.FX(s,j,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      GC.FX(j,v,r,t)$(t.val > tmyopicUP)               = eps ;
$if      set myopic      IG.FX(j,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      GB.FX(s,j,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      SC.FX(r,t)$(t.val > tmyopicUP)                   = eps ;
$if      set myopic      DA.FX(r,t)$(t.val > tmyopicUP)                   = eps ;
$if      set myopic      TC.FX(transmission,r,rr,t)$(t.val > tmyopicUP)   = eps ;

* In general, there is no lignite and oilother allowed from 2020 onwards
$if      set nolign      IX.FX("Lignite",r,"2020")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2025")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2030")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2035")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2040")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2045")                       = eps ;
$if      set nolign      IX.FX("Lignite",r,"2050")                       = eps ;
$if      set nooil       IX.FX("OilOther",r,"2020")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2025")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2030")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2035")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2040")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2045")                      = eps ;
$if      set nooil       IX.FX("OilOther",r,"2050")                      = eps ;

* Debugging optimality and speeding up the model (in particular, for some mypopic runs)
$if      set help        IX.FX("Bioenergy",r,"2045")                     = eps ;
$if      set help        IX.FX("Lignite",r,"2045")                       = eps ;
$if      set help        IX.FX("Coal",r,"2045")                          = eps ;
$if      set help        IX.FX("Coal_CCS",r,"2045")                      = eps ;
$if      set help        IX.FX("Geothermal",r,"2045")                    = eps ;
$if      set help        IX.FX("OilOther",r,"2045")                      = eps ;

$if      set help        IX.FX("Bioenergy",r,"2050")                     = eps ;
$if      set help        IX.FX("Lignite",r,"2050")                       = eps ;
$if      set help        IX.FX("Coal",r,"2050")                          = eps ;
$if      set help        IX.FX("Coal_CCS",r,"2050")                      = eps ;
$if      set help        IX.FX("Geothermal",r,"2050")                    = eps ;
$if      set help        IX.FX("OilOther",r,"2050")                      = eps ;

* Upper bound on dedicated biomass supply (depends on biomass carbon price scenario)
$if      set biomark_r   DBSR.UP(dbclass,r,t)    = dblim_r(dbclass,r,t) ;
$if      set biomark     DBS.UP(dbclass,t)       = dblim(dbclass,t) ;
$if      set gasmark_r   NGSR.UP(ngclass,r,t)    = nglim_r(ngclass,r,t);
$if      set gasmark     NGS.UP(ngclass,t)       = nglim(ngclass,t);

* * * * * Model declaration and solution
model euregen /
objdef
* * * Demand equations
demand
$if set rsa                                      demand_rsa
demand_ann
* Generation
$if not  set mustrun                             capacity
*MM (todo): The mustrun conditions lead to (numerical) infeasibilities in the moment (need to check)
$if      set mustrun                             capacity_bio
$if      set mustrun                             capacity_dsp
$if      set mustrun                             capacity_nsp
$if      set nucmin                              capacity_nucmin
$if      set techmin                             capacity_techmin
$if      set cofiring                            capacity_cofir
invest
exlife2015
$if      set corona                              exlife2020
exlife
$if      set corona                              exlife_bio
$if      set retrofit                            retro
newlife
$if not  set myopic                              retire
investlimUP
investlimLO
$if      set limeu                               investlimUP_eu
investlimUP_irnw
* * * Storage
$if      set storage                             ginvest
$if      set storage                             gexlife
$if      set storage                             gexlife2015
$if      set storage     $if set corona          gexlife2020
$if      set storage     $if set myopic          gexlife_pump
$if      set storage                             gnewlife
$if      set storage     $if not set myopic      gretire
$if      set storage                             chargelim
$if      set storage                             dischargelim
$if      set storage                             storagebal
$if      set storage                             storagelim
$if      set storage                             ginvestlimUP
$if      set storage                             ginvestlimLO
$if      set storage     $if set glimeu          ginvestlimUP_eu
* Transmission
$if      set trans                               tcapacity
$if      set trans                               tinvest
$if      set trans                               tinvestlimUP
$if      set trans                               tinvestlimLO
$if      set trans       $if set tlimeu          tinvestlimUP_eu
* Markets
$if      set biomark                             biomarket
$if      set biomark_r                           biomarket_r
$if      set gasmark                             gasmarket
$if      set gasmark_r                           gasmarket_r
* EU ETS and MSR (deactivation of MSR via excel)
$if      set euets                               euets_market
$if      set euets                               euets_co2elec
$if      set euets                               euets_co2ind
$if      set euets                               euets_itnbc
$if      set euets                               euets_cbcdf
* Policy
$if      set rnwtarget                           rnwtgtmarket
$if      set rnwtarget_r                         rnwtgtmarket_r
$if      set irnwtarget                          irnwtgtmarket
$if      set irnwtarget_r                        irnwtgtmarket_r
$if      set co2cap                              co2market
$if      set co2cap      $if set banking         itnbc
$if      set co2cap      $if set banking         cbcdf
$if      set co2cap_r                            co2market_r
$if      set coalexit                            coalphaseout
$if      set coalexit_r                          coalphaseout_r
$if      set lignexit                            lignphaseout
$if      set lignexit_r                          lignphaseout_r
$if      set nucexit                             nucphaseout
$if      set nucexit_r                           nucphaseout_r
* Limits
$if      set biolim                              cumbio
$if      set biolim_r                            cumbio_r
$if      set sclim                               cumccs
$if      set sclim                               ccsflow
$if      set sclim_r                             cumccs_r
$if      set sclim_r                             ccsflow_r
* * * Structural equations to aid solver
xtwhdef
copyxc
/;

* Intialize co2 market to ensure report compiles even when the constraint is excluded
co2market.M(t) = 0 ;
co2market_r.M(r,t) = 0 ;
euets_market.M(t) = 0 ;

$if not set solver $set solver gurobi
option qcp=%solver%;

euregen.optfile = 1;
euregen.holdfixed = 1;
*euregen.reslim = 7200;
euregen.reslim = 604800;
option solprint = on;

solve euregen using qcp minimizing SURPLUS;

*Don't include report so that restart file can be used with modified report without re-running model

