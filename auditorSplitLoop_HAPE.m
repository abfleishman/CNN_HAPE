
% hape=submitJob('fig2csvBatch',{'/mnt/NAS3_2Mar15/All_HAPE/HAPE2csv.txt','/mnt/NAS3_2Mar15/All_HAPE','','site_yyyymmdd_HHMMSS'},0);

% job=submitJob( 'collectAuditInfo',{'/mnt','/mnt/NAS1_2Jun14/figDb.csv','fig'},0);

hape = readCsvColumns('\\NAS1\NAS1_2Jun14\Motherships\HAPE_CNN_Mother_test\hape_streams.csv',true,'audit_file','nneg','npos','outpath', 'outdir');
path = cellfun(@(x) x(1), hape);
nneg = cellfun(@(x) x(2), hape);
npos = cellfun(@(x) x(3), hape);
outpath = cellfun(@(x) x(4), hape);
outdir = cellfun(@(x) x(5), hape);

failedpaths =   num2str(zeros(length(npos),1));

for i = 1:length(hape)
    [~,~]=mkdir(char(outdir(i)));
    [num2str(i),': ' ,char(path(i))]
    if str2num(char(npos(i))) == 0 & str2num(char(nneg(i))) > 0
        auditorSplit(strrep(char(path(i)),'/','\'),  {char(outpath(i))},  '', [str2num(char(nneg(i)))], 'uniform');
    else
        if str2num(char(npos(i))) > 0 & str2num(char(nneg(i))) > 0
           auditorSplit(strrep(char(path(i)),'/','\'),  {char(outpath(i))},  '', [str2num(char(nneg(i)))],'5', [str2num(char(npos(i)))],'uniform');
        else
            if str2num(char(npos(i))) > 0 & str2num(char(nneg(i))) == 0
               auditorSplit(strrep(char(path(i)),'/','\'),  {char(outpath(i))},'5', [str2num(char(npos(i)))],'uniform');
            else
                warning(['Failed: ' i path(i)])
                failedpaths(i) = path(i)
            end
        end
    end
end

fileStreamFromFigs(char(outpath(i)),........)
