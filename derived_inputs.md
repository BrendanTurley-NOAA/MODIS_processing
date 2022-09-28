---
editor_options: 
  markdown: 
    wrap: 72
---

### Carder bbp - 547nm wavelength, total backscatter to Morel backscatter (Carder et al. 1999; Cannizaro et al. 2008)

Carder_bbp(555) = −0.00182 + 2.058 \* rsr(555)

### Morel bbp - 547nm wavelength (Morel 1988)

Morel_bbp(555) = (0.3*(chl)\^0.62)*(0.002 + 0.02 \* (0.5 − 0.25 \*
log10(chl)))

### CMbbp ratio - 547nm wavelength (Tomlinson et al. 2009)

Carder_bbp(555)/Morel_bbp(555) \<1.0 = Kbrevis bloom (Cannizaro 2008);
used chl_oc4 \<2.0 = Kbrevis bloom (Tomlinson et al. 2009); used Stumpf
et al 2000 necessitating change

### ssnLw488 - spectral shape at 488nm (Wynne et al . 2008; assume not significantly different from ssnLw490, Tomlinson et al. 2009)

ss(lambda) = nLw(lambda) - nLw(lambda\_-) - (nLw(lambda\_+) -
nLw(lambda\_-)) \* (lambda - lambda\_-)/(lambda\_+ - lambda\_-)
lambda\_- is next lower bandwidth from lambda lambda\_+ is next higher
bandwidth from lambda negative ss(490) = Kbrevis bloom

### benthic resuspension of chl

chl(res) \~ rsr(670) anomaly calculated similar to chl anomaly then
multiply rsr(670)\_anom by 200 ug sr l-1 to convert ro chlorophyll
(Wynne et al. 2005, 2006) corrected_chlorophyll_anomaly =
original_chl_anomaly - chl(res) standard chl_oc4 overestimates
chlorophyll in coastal FL; use Stumpf et al. 2000

### Kbrevis ensemble

inputs: chl_anom, ss(490), CMbbp if all three detect = red if chl_anom
and ss(490) or CMbbp = turquoise or green, respectively if chl_anom
exclusively = blue

### Fluorescent line height derived product

threshold of above 0.12 W m−2 μm−1 sr−1 were associated with K. brevis
concentration between 104 and 106 cells L−1 (Hu et al. 2005) FLH anomaly
maybe suitable replacement for chl_anom (Tomlinson et al. 2009)
