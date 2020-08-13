%Code for Calculating the total duration of "on" effort for a cruise, based
%on .xls effort logs. 
%KMP 181127

close all
clearvars

%First, select the directory, then open each file, calculate the duration
%and save with file name, and finally export all the durations for all the
%files in that directory.

%Start off with selecting your directory
%'Select the directory that contains the effort information');
effsheetpath = ('C:\Users\KMERKENS\Documents\CRP\TowedArrayCalcTest_181127\');
%list all the .xlsx files there
efffiles = dir(fullfile(effsheetpath, '*.xlsx'));
%Or just hardcode a single file, if that's all you need
%efffiles = 'C:\Users\KMERKENS\Documents\CRP\TowedArrayCalcTest_181127\HICEAS2010_Sette_Effort.xlsx';

%preallocate empty matricies to hold results.
filenames = [];
filetotals = [];

for i = 1:size(efffiles,1)
  %Read file - header row becomes row 1 of effraw
  [effnums, effstrings, effraw] = xlsread(fullfile(effsheetpath, efffiles(i).name));
  %Go row by row and for each row, ask whether the effort is on or off
  %Preallocate a new vector for listing on effort start times and durations
  effortstart = [];
  alloneffortdurs = [];
  %Indexing vector for skipping over multiple on effort rows
  y = 1;
  for r = 1:size(effraw,1);
    effort = effraw(r,5);
    %is that on effort
    oneffort = ismember(effort,'on');
    %If it's on, check whether there is already a start
    if oneffort == 1
        %If there isn't one (i.e. this is the first row of this on effort
        %period)
        if isempty(effortstart)
            %save the start time
            effortstart = effraw(r,2);
            %Then skip ahead to the next row
            continue
        else
            %just skip ahead to the next row
            continue
        end
    %If this one is off effort
    elseif oneffort == 0
        %Check to see if there is a start time logged, if not, skip to next
        %row, if so, 
        if isempty(effortstart)
            continue
        else %If there is a start log, this must be the end of that start
            effortend = effraw(r,2);
            %Convert both start and end times into datenums
            effortstartnum = datenum(effortstart);
            effortendnum = datenum(effortend);
            %Calculate the total duration
            totalduration = effortendnum - effortstartnum;
            alloneffortdurs = [alloneffortdurs;totalduration];
            %Then erase the logged start time
            effortstart = [];
            %And continue on to the next loop
        end
    end
  end
  %Then get the sum for this whole file
  totalon = sum(alloneffortdurs);
  %add file name to list of names
  %find the name of the file, truncated to 15 characters for simplicity 
  thisname = efffiles(i).name(1:15);
  filenames = [filenames; thisname];
  %Add file sum total to list of totals
  filetotals = [filetotals;totalon]; 
end
      
      %Make it into one cell array
cellnames = cellstr(filenames);
celltots = num2cell(filetotals);
output = {cellnames, celltots};
%Write to xls.
xlswrite(fullfile(effsheetpath,'TotalOnEffort.xls'),[cellnames,celltots]);



%%%%%%%%Notes from testing other ideas below%%%%%%%%%

%     %Then check the next row to see if it's off
%     nexteffort = effraw(r+1,5);
%     nextoneffort = ismember(nexteffort, 'on');
%     %If it's not on effort, use that as the end time
%     if nextoneffort == 0
%         effortend = effraw(r+1,2);
%         %Convert start and end times to datenums
%         effortstartnum = datenum(effortstart);
%         effendnum = datenum(effortend);
%         %Calculate effor time
%         efforttotal = effortendnum-effotrstartnum;
%         oneffortsums = [oneffortsums; efforttotal];
%     %If the next one is on effort, skip ahead to the next row    
%     elseif nextoneffort == 1
        
        
       
        
  
  
%   %Go to Effort column
%   effort = effraw(1:end,5);
%   %Find cells containing "on" effort
%   oneffort = find(ismember(effort,'on'));
%   %For those indices, get the start and end times (don't use column 1,
%   %which has some errors, use colum 2)
%   startsall = effraw(1:end,2);
%   %Select only those that were on effort
%   startson = startsall(oneffort);
%   %Convert to vector of datenums
%   datenumstart = datenum(startson);
%   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   %%%%Waiting here for Yvonne to send end times
%   %Now get the end times
%   endsall = effraw(1:end,2);
%   %Select only those on effort
%   endson = endsall(oneffort);
%   %Convert to vector of datenums
%   datenumend = datenum(endson);
%   %Subtract starts from ends to get durations
%   duron = datenumend - datenumstart;
%   %Get sum of all on effort periods
%   totalon = sum(duron);
%   %add file name to list of names
%   %find the name of the file, truncated to 15 characters for simplicity 
%   thisname = efffiles(i).name(1:15);
%   filenames = [filenames, thisname];
%   %Add file sum total to list of totals
%   filetotals = [filetotals,totalon];  









