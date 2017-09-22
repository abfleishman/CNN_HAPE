
hape = readCsvColumns('D:\CM,Inc\CNN_HAPE\hape_streams.csv',true,'audit_file','nneg','npos','outpath', 'outdir');
path = cellfun(@(x) x(1), hape);
nneg = cellfun(@(x) x(2), hape);
npos = cellfun(@(x) x(3), hape);
outpath = cellfun(@(x) x(4), hape);
outdir = cellfun(@(x) x(5), hape);

failedpaths = zeros(808,1);
[~,~]=mkdir(outdir);

for i = 1:length(hape)
    [~,~]=mkdir(char(outdir(i)));
    warning([num2str(i),': ' ,char(path(1))])
    if str2num(char(npos(i))) == 0 & str2num(char(nneg(i))) > 0
        auditorSplit(char(path(i)),  {char(outpath(i))},  '', [str2num(char(nneg(i)))], 'uniform');
    else
        if str2num(char(npos(i))) > 0 & str2num(char(nneg(i))) > 0
           auditorSplit(char(path(i)),  {char(outpath(i))},  '', [str2num(char(nneg(i)))],'5', [str2num(char(npos(i)))],'uniform');
        else
            if str2num(char(npos(i))) > 0 & str2num(char(nneg(i))) == 0
               auditorSplit(char(path(i)),  {char(outpath(i))},'5', [str2num(char(npos(i)))],'uniform');
            else
                warning(['Failed: ' i path(i)])
                failedpaths(i) = path(i)
            end
        end
    end
end


            
   

auditorMerge({char(outpath(1)), char(outpath(2)), char(outpath(3))},'\\NAS1\NAS1_2Jun14\Motherships\HAPE_CNN_Mother_test\smoosh.fig')

auditor('\\NAS1\NAS1_2Jun14\Motherships\HAPE_CNN_Mother_test\KESRP_HNKP_2015_R2\HNKP3_28October15','HNKP3_28October15_audit_HAPE_21Apr')
auditorSaveProject('\\NAS1\NAS1_2Jun14\Motherships\HAPE_CNN_Mother_test\KESRP_HNKP_2015_R2\HNKP3_28October15','HNKP3_28October15_audit_HAPE_21Apr')