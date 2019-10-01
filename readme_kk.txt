Addition of Calcifiers and Prognostic Calcite to ocean model
April 2015
Karin Kvale
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Full documentation of the changes made to the source code can be found in:

Kvale et al. (2015) Explicit planktic calcifiers in the University of Victoria Earth System Climate Model version 2.9. Atmosphere-Ocean.


Files modified from the base version have been pulled out into updates/

There are 3 options:

O_cal_kk
This option turns on the calcifier phytoplankton type

O_kk_caco3tr
This option turns on prognostic calcite. For this option to work, O_sed must also be activated.

O_kk_ballast
This option turns on ballasting of organic carbon by calcite. Please note you cannot use this option without O_kk_caco3tr!

IMPORTANT: The version contained in SVN is not the version used in the model description paper. A separate branch of the model code (from c.~2011) was used for that paper and included several bugs that affect the biology. The calcifier and calcite options here were added to the trunk version Spring 2015 and have not been thoroughly tested or assessed (though differences are unlikely to be large). It is possible some parameter values may require adjustment to achieve results like those found in the model description paper.
