
% changeWavDirBatch('main dir where figs live', 'OldStr and NewStr are the same?', 'no matcher', oldExt = wav, newExt = flac)

changeWavDirBatch('\\NAS1\NAS1_2Jun14\Figs\IC_Pinzon_2012','NAS1_2Jun14\Sounds\IC_Pinzon_2012','NAS2_9Oct14\Sounds\IC_Pinzon_2012_FLAC','','wav', 'flac')

%% check to make sure it's working
auditor('\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2013_r1\KESRP_ULP_HONO_2013_r1_ULP1_Apr18', 'r1_ULP1_Apr18_DeadMic_Test_dnnClass_audit_HAPE_18Jun14')

guiData = get(1, 'UserData');

delete(gcf);delete(gcf);delete(gcf);

%%

% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_HNKP_2015_R2'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC_SPLIT'

% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_HNKP_2015_R2'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC_SPLIT_SPLIT\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC_SPLIT\'

% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_HNKP_2015_R2_RIGHT'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC_RIGHT\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_HNKP_2015_R2_FLAC_SPLIT_BACKUP\'
 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_Lehua_2013_r1'
% OldPath = '\\shmoo\Raided_Data_2\KESRP_Lehue_2013_R1\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_Lehua_2013_R1_FLAC\'

% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_Lehua_2013_r1'
% OldPath = 'A:\Project_Data\2013\KESRP_Lehue_2013_R1\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_Lehua_2013_R1_FLAC\'

% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_Lehua_2013_r1'
% OldPath = '\\SHMOO\Raided_Data_2\KESRP_Lehue_2013_R1\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_Lehua_2013_R1_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_Lehua_2014_r2'
% OldPath = '\\SHMOO\2014_Single_1\Sounds\KESRP_Lehua_2014_r2\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_Lehua_2013_R2_FLAC\'
%% started here, working downward. TEST SOME OF THESE TO MAKE SURE THEY WORKED!!
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULP_2012'
% OldPath = '\\NAS1\NAS2_9Oct14\Sounds\KESRP_ULP_2012\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_ULP_2012_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2013'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2013\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_ULPHONO_2013_FLAC\'
% xx
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2013_r1'
% OldPath = 'A:\Project_Data\2013\KIUC_ULP_2013\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\KESRP_ULPHONO_2013_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R3'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\FIWI_2014_r1\'
% NewPath = '\\NAS1\NAS3_2Mar15\Sounds\FIWI_2014_R1_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R3'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R3\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R3_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R4'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R4\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R4_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R5'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R5\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R5_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R6'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R6\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R6_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R7'
% OldPath = '\\NAS1\NAS2_9Oct14\Sounds\KESRP_ULPHONO_2014_R7\'
% NewPath = '\\NAS1\NAS2_9Oct14\Sounds\KESRP_ULPHONO_2014_R7_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_ULPHONO_2014_R8'
% OldPath = '\\NAS1\NAS2_9Oct14\Sounds\KESRP_ULPHONO_2014_R8\'
% NewPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_ULPHONO_2014_R8_FLAC\'
% 
% FigDir = '\\NAS1\NAS1_2Jun14\Figs\KESRP_UMP_2014_R3'
% OldPath = '\\NAS1\NAS1_2Jun14\Sounds\KESRP_UMP_2014_R3_WAV_BACKUP\'
% NewPath = '\\NAS1\NAS2_9Oct14\Sounds\KESRP_UMP_2014_R3_FLAC_BACKUP\'
% 
FigDir = '\\NAS1\NAS1_2Jun14\Figs\MNSRP_Kahikinui_2014_r1_WAV_22050'
OldPath = '\\NAS1\NAS1_2Jun14\Sounds\MNSRP_Kahikinui_2014_r1_WAV_22050\'
NewPath = '\\NAS1\NAS3_2Mar15\Sounds\MNSRP_Kahikinui_2014_R1_FLAC\'

changeWavDirBatch(FigDir, OldPath, NewPath, '', 'wav', 'flac')