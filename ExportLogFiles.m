cd 'D:\CM,Inc\CMIAuditor'
auditorPath

% HAPE - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_HAPE\HAPE2csv_more_20Feb18.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_HAPE')
matcher = '';
fileNameFmt = 'site_yyyymmdd_HHMMSS';
info = fig2csvBatch(mainDir, outDir, matcher, fileNameFmt)

% NESH - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_NESH\NESH2csv.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_NESH')
matcher = '';
% fileNameFmt = {'site_yyyymmdd_HHMMSS','site_x_yyyymmdd_HHMMSS'};
info = fig2csvBatch(mainDir, outDir, matcher)

% BAOW - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_BAOW\BAOW2csv.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_BAOW')
matcher = '';
% fileNameFmt = {'site_yyyymmdd_HHMMSS','site_x_yyyymmdd_HHMMSS'};
info = fig2csvBatch(mainDir, outDir, matcher)

% BUPE - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_BUPE\BUPE2csv.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_BUPE')
matcher = '';
fileNameFmt = {'site_yyyymmdd_HHMMSS','site_x_yyyymmdd_HHMMSS'};
info = fig2csvBatch(mainDir, outDir, matcher)

% WTSH - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_WTSH\WTSH2csv.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_WTSH')
matcher = '';
% fileNameFmt = {'site_yyyymmdd_HHMMSS','site_x_yyyymmdd_HHMMSS'};
info = fig2csvBatch(mainDir, outDir, matcher)

% BANP - done!
mainDir = convertPath('\\NAS1\NAS3_2Mar15\All_BANP\BANP2csv.txt')
outDir = convertPath('/mnt/NAS3_2Mar15/All_BANP')
matcher = '';
% fileNameFmt = {'site_yyyymmdd_HHMMSS','site_x_yyyymmdd_HHMMSS'};
info = fig2csvBatch(mainDir, outDir, matcher)
