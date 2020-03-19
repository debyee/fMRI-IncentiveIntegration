function LF_formatdata
%% Function that formats the behavioral liquid feedback data into a table 
% INPUT: need to update the subject numbers, and identify whether the
% second baseline was completed by the subject or not.
%
% OUTPUT: writes a table for R analysis
% 
% NOTES ABOUT DATA
% This script refers to the data on the Box Folder.
% Subjects 1-17: only one baseline run, and criterion RT is calculated
% based on the 48 trials from one baseline run.
% Subjects 18-51: changed to two baseline runs prior to incentive runs, and
% criterion RT is calculated based on the 96 trials across these two runs.
%
% Last updated by Debbie Yee, 1/8/2018.

clear all; 
runs_baseline_s1_s17 = [8 15];
runs_baseline_s18_s51 = [7 8];
runs_incentive = (9:14);
% excluding subjects 5,22,34 because they did not complete task
subjects = {'2','3','4','6','7','8','9','10','11','12','13','14','15','16','17', ...
    '18','19','20','21','23','24','25','26','27','28','29','30','31','32','33', ...
    '35','36','37','38','39','40','41','42','43','44','45','46','47','48', '49', ...
    '50','51'};
% completed second baseline, only relevant for Subjects 1-17
base2_complete = {'4','6','7','8','10','11','12','13','14','15','16','17'};

%% Creating a table of subject data for each trial, as well as information presented on each trial 

for i = 1:length(subjects)
    
    % BASELINE
    %%% SUBJECTS 1-17 %%%
    if str2double(subjects{i})<=17
        
        % load subject data for run8 (baseline 1)
        if str2double(subjects{i})<10
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run8_mixed.mat']);
        else
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run8_mixed.mat']);
        end
        
        % Identify relevant variables for baseline 1
        sid = cellstr(repmat(subjects{i},48,1));
        runID = repmat(num2cell(8),48,1);
        runType = repmat(cellstr('baseline'),48,1);
        trial = num2cell((1:48))';
        %formatting RT data -- account for missing data when formatting RT
        RT = data.SubjectResponses(:,1);
        emptycells = cellfun(@isempty,RT);
        RT(emptycells) = {NaN};
        RT = num2cell(round(cell2mat(RT)*1000,3));
        Feedback = data.SubjectResponses(:,5);
        emptycells = cellfun(@isempty,Feedback);
        Feedback(emptycells) = {'none'};
    
        % Stimulus presented on left and right of screen during target trial
        no_trials = 48;
        target_left = cell(no_trials,1);
        target_right = cell(no_trials,1);
        for t = 1:no_trials;
            if ( length(data.list_taskfMRI{t,8}) == 1 && isnan(data.list_taskfMRI{t,8}) )
                target_left{t} = NaN;
                target_right{t} = NaN;
            else
                target_left{t}=data.list_taskfMRI{t,8}{1,1};
                target_right{t}=data.list_taskfMRI{t,8}{1,2};
            end
        end
    
        % create table for baseline 1 run
        T1 = table(sid,runID,runType,trial,RT,data.SubjectResponses(:,2),data.SubjectResponses(:,3),data.SubjectResponses(:,4),Feedback, ...
            data.list_taskfMRI(:,2), data.list_taskfMRI(:,3),data.list_taskfMRI(:,4),data.list_taskfMRI(:,5),data.list_taskfMRI(:,6),data.list_taskfMRI(:,7), ...
            target_left, target_right, data.list_taskfMRI(:,9),data.list_taskfMRI(:,10));
        T1.Properties.VariableNames = {'subID','runID','runType','trial','RT','ACC','ButtonPress','CorrectButtonPress','Feedback', ...
            'switch_trial','task','congruency','left_item','task_ifletter','task_ifdigit', ...
            'target_left','target_right','moneyreward','moneysymbol'};
        
        % concatentate subject data with grand table
        if (i==1)
            T_trialdata = T1;
        else
            T_trialdata = vertcat(T_trialdata,T1);
        end
        
    %%% SUBJECTS 18-51 %%%
    elseif str2double(subjects{i})>17
        for r = 1:length(runs_baseline_s18_s51)
            
            % load subject baseline data
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run',num2str(runs_baseline_s18_s51(r)),'_mixed.mat']);
            
            % Identify relevant variables for baseline 1
            sid = cellstr(repmat(subjects{i},48,1));
            runID = repmat(num2cell(runs_baseline_s18_s51(r)),48,1);
            runType = repmat(cellstr('baseline'),48,1);
            trial = num2cell((1:48))';
            %formatting RT data -- account for missing data when formatting RT
            RT = data.SubjectResponses(:,1);
            emptycells = cellfun(@isempty,RT);
            RT(emptycells) = {NaN};
            RT = num2cell(round(cell2mat(RT)*1000,3));
            Feedback = data.SubjectResponses(:,5);
            emptycells = cellfun(@isempty,Feedback);
            Feedback(emptycells) = {'none'};
            
            % Stimulus presented on left and right of screen during target trial
            no_trials = 48;
            target_left = cell(no_trials,1);
            target_right = cell(no_trials,1);
            for t = 1:no_trials;
                if ( length(data.list_taskfMRI{t,8}) == 1 && isnan(data.list_taskfMRI{t,8}) )
                    target_left{t} = NaN;
                    target_right{t} = NaN;
                else
                    target_left{t}=data.list_taskfMRI{t,8}{1,1};
                    target_right{t}=data.list_taskfMRI{t,8}{1,2};
                end
            end
            
            % create table for baseline 1 run
            T1 = table(sid,runID,runType,trial,RT,data.SubjectResponses(:,2),data.SubjectResponses(:,3),data.SubjectResponses(:,4),Feedback, ...
                data.list_taskfMRI(:,2), data.list_taskfMRI(:,3),data.list_taskfMRI(:,4),data.list_taskfMRI(:,5),data.list_taskfMRI(:,6),data.list_taskfMRI(:,7), ...
                target_left, target_right, data.list_taskfMRI(:,9),data.list_taskfMRI(:,10));
            T1.Properties.VariableNames = {'subID','runID','runType','trial','RT','ACC','ButtonPress','CorrectButtonPress','Feedback', ...
                'switch_trial','task','congruency','left_item','task_ifletter','task_ifdigit', ...
                'target_left','target_right','moneyreward','moneysymbol'};
            
            % concatentate subject data with grand table
            if (i==1)
                T_trialdata = T1;
            else
                T_trialdata = vertcat(T_trialdata,T1);
            end
            
        end
        
    %%% ERROR IF PROBLEM LOADING BASELINE DATA FOR ANY SUBJECT
    else
        disp(['Error loading subject baseline data for subject ' subjects{i}]);
    end
        
    
    % INCENTIVE / REWARD RUNS (WITH LIQUID FEEDBACK) 
    for r = 1:length(runs_incentive)
        
        % subject 39 doesnt have a last run 14, so need to skip that iteration
        % for that run. The rest of the subjects have 6 incentive runs.
        if (str2double(subjects{i})==39) && (r==6)
            continue;
        else
            %load the subject incentive data
            if str2double(subjects{i})<10
                %load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
                load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
            elseif str2double(subjects{i})>=10
                %load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
                load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
            else
                disp(['Error loading subject incentive data for subject ' subjects{i}]);
            end
            
            % identify relevant variables for incentive run table
            sid = cellstr(repmat(subjects{i},48,1));
            runID = repmat(num2cell(runs_incentive(r)),48,1);
            runType = repmat(cellstr('incentive'),48,1);
            trial = num2cell((1:48))';
            %formatting RT data -- account for missing data when formatting RT
            RT = data.SubjectResponses(:,1);
            emptycells = cellfun(@isempty,RT);
            RT(emptycells) = {NaN};
            RT = num2cell(round(cell2mat(RT)*1000,3));
            
            % NOTE: for subject 3 (i==2), stopped midway of run 12 (4th run), so need to make trials 33 and higher all NaNs
            if (i==2 && r==4)
                data.list_taskfMRI(33:48,2:10) = {NaN};
            end
            
            %stimulus presented on left and right of screen during target trial
            no_trials = 48;
            target_left = cell(no_trials,1);
            target_right = cell(no_trials,1);
            for t = 1:no_trials;
                if ( length(data.list_taskfMRI{t,8}) == 1 && isnan(data.list_taskfMRI{t,8}) )
                    target_left{t} = NaN;
                    target_right{t} = NaN;
                else
                    target_left{t}=data.list_taskfMRI{t,8}{1,1};
                    target_right{t}=data.list_taskfMRI{t,8}{1,2};
                end
            end
            
            % create table for incentive run
            T2 = table(sid,runID,runType,trial,RT,data.SubjectResponses(:,2),data.SubjectResponses(:,3),data.SubjectResponses(:,4),data.SubjectResponses(:,5), ...
                data.list_taskfMRI(:,2), data.list_taskfMRI(:,3),data.list_taskfMRI(:,4),data.list_taskfMRI(:,5),data.list_taskfMRI(:,6),data.list_taskfMRI(:,7), ...
                target_left, target_right, data.list_taskfMRI(:,9),data.list_taskfMRI(:,10));
            T2.Properties.VariableNames = {'subID','runID','runType','trial','RT','ACC','ButtonPress','CorrectButtonPress','Feedback', ...
                'switch_trial','task','congruency','left_item','task_ifletter','task_ifdigit', ...
                'target_left','target_right','moneyreward','moneysymbol'};
            
            % concatentate subject data with grand table
            T_trialdata = vertcat(T_trialdata,T2);
        end
    end    
end



% write trial table to directory
%writetable(T_trialdata,'/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Trimmed/subject_data.txt','Delimiter','\t');
writetable(T_trialdata,'/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Trimmed/subject_data.txt','Delimiter','\t');
disp('Completed creating table of trial data for all subjects, and saved to computer.');

%% Creating a Tables out Stimuli Times for Cues and Targets for all runs

for i = 1:length(subjects)
    
    % BASELINE
    %%% SUBJECTS 1-17 %%%
    if str2double(subjects{i})<=17
        
        % load subject data for run8 (baseline 1)
        if str2double(subjects{i})<10
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run8_mixed.mat']);
        else
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run8_mixed.mat']);
        end
        
        % identify relevant variables for stimulus onsets in incentive runs
        sid=cellstr(repmat(subjects{i},292,1));
        runID=repmat(num2cell(8),292,1);
        runType=repmat(cellstr('baseline'),292,1);
        tmp_onsets = data.onsets(1:292,:);
        emptycells = cellfun(@isempty,tmp_onsets);
        tmp_onsets(emptycells) = {NaN};
        
        % create table for baseline run
        T4 = table(sid,runID,runType,tmp_onsets(:,1),tmp_onsets(:,2),tmp_onsets(:,3),tmp_onsets(:,4),tmp_onsets(:,5),tmp_onsets(:,6));
        T4.Properties.VariableNames = {'subID','runID','runType','trial','stim_present','onset_calc','onset_exp','comp_onset_calc','comp_onset_exp'};
        
        % concatentate subject data with grand table
        if (i==1)
            T_stimdata = T4;
        else
            T_stimdata = vertcat(T_stimdata,T4);
        end
        
    %%% SUBJECTS 18-51 %%%
    elseif str2double(subjects{i})>17
        for r = 1:length(runs_baseline_s18_s51)
            
            % load subject baseline data
            load(['/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run8_mixed.mat']);
            
            % identify relevant variables for stimulus onsets in incentive runs
            sid=cellstr(repmat(subjects{i},292,1));
            runID=repmat(num2cell(runs_baseline_s18_s51(r)),292,1);
            runType=repmat(cellstr('baseline'),292,1);
            tmp_onsets = data.onsets(1:292,:);
            emptycells = cellfun(@isempty,tmp_onsets);
            tmp_onsets(emptycells) = {NaN};
            
            % create table for baseline run
            T4 = table(sid,runID,runType,tmp_onsets(:,1),tmp_onsets(:,2),tmp_onsets(:,3),tmp_onsets(:,4),tmp_onsets(:,5),tmp_onsets(:,6));
            T4.Properties.VariableNames = {'subID','runID','runType','trial','stim_present','onset_calc','onset_exp','comp_onset_calc','comp_onset_exp'};
            
            % concatentate subject data with grand table
            if (i==1)
                T_stimdata = T4;
            else
                T_stimdata = vertcat(T_stimdata,T4);
            end
            
        end
        
    %%% ERROR IF PROBLEM LOADING BASELINE DATA FOR ANY SUBJECT
    else
        disp(['Error loading subject baseline data for subject ' subjects{i}]);
    end
       
    % INCENTIVE / REWARD RUNS (WITH LIQUID FEEDBACK)
    for r = 1:length(runs_incentive)
        
        % subject 39 doesnt have a last run 14, so need to skip that iteration
        % for that run. The rest of the subjects have 6 incentive runs.
        if (str2double(subjects{i})==39) && (r==6)
            continue;
        else            
            %load the subject data
            if str2double(subjects{i})<10
                load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
            elseif str2double(subjects{i})>=10
                load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run',num2str(runs_incentive(r)),'_mixed.mat']);
            else
                disp(['Error loading subject incentive data for subject ' subjects{i}]);
            end
            
            %identify relevant variables for stimulus onsets in incentive runs
            sid=cellstr(repmat(subjects{i},292,1));
            runID=repmat(num2cell(runs_incentive(r)),292,1);
            runType=repmat(cellstr('incentive'),292,1);
            tmp_onsets = data.onsets(1:292,:);
            emptycells = cellfun(@isempty,tmp_onsets);
            tmp_onsets(emptycells) = {NaN};
            T5 = table(sid,runID,runType,tmp_onsets(:,1),tmp_onsets(:,2),tmp_onsets(:,3),tmp_onsets(:,4),tmp_onsets(:,5),tmp_onsets(:,6));
            T5.Properties.VariableNames = {'subID','runID','runType','trial','stim_present','onset_calc','onset_exp','comp_onset_calc','comp_onset_exp'};
            
            % concatentate subject data with grand table
            T_stimdata = vertcat(T_stimdata,T5);
        end   
    end
end

% write incentive table to directory
writetable(T_stimdata,'/Users/debbieyee/Box Sync/Aging Motivation (SRNDNA)/Data/Trimmed/stim_present_data.txt','Delimiter','\t');
disp('Completed creating table of stimulus presentations for all subjects, and saved to computer.');


%% Extra Code for Baseline 2
% IN THE FIRST FORLOOP
%     % Baseline 2 run (if subject completed the second baseline)
%     if ismember(base2_complete,subjects{i}) == 1 
%         
%         %load subject data for run15 (baseline 2)
%         if str2double(subjects{i})<10
%             load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run15_mixed.mat']);
%         else
%             load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run15_mixed.mat']);
%         end
%         
%         % identify relevant variables for baseline 2
%         sid = cellstr(repmat(subjects{i},48,1));
%         runID = repmat(num2cell(15),48,1); 
%     	runType = repmat(cellstr('baseline'),48,1);
%         trial = num2cell((1:48))';
%         %formatting RT data -- account for missing data when formatting RT
%         RT = data.SubjectResponses(:,1);
%         emptycells = cellfun(@isempty,RT);
%         RT(emptycells) = {NaN};
%         RT = num2cell(round(cell2mat(RT)*1000,3));
%         Feedback(emptycells) = {'none'};
%         
%         %stimulus presented on left and right of screen during target trial
%         no_trials = 48;
%         target_left = cell(no_trials,1);
%         target_right = cell(no_trials,1);
%         for t = 1:no_trials;
%             if ( length(data.list_taskfMRI{t,8}) == 1 && isnan(data.list_taskfMRI{t,8}) )
%                 target_left{t} = NaN;
%                 target_right{t} = NaN;
%             else
%                 target_left{t}=data.list_taskfMRI{t,8}{1};
%                 target_right{t}=data.list_taskfMRI{t,8}{2};
%             end
%         end
%         
%         % create table for baseline run
%         T3 = table(sid,runID,runType,trial,RT,data.SubjectResponses(:,2),data.SubjectResponses(:,3),data.SubjectResponses(:,4),Feedback, ...
%             data.list_taskfMRI(:,2), data.list_taskfMRI(:,3),data.list_taskfMRI(:,4),data.list_taskfMRI(:,5),data.list_taskfMRI(:,6),data.list_taskfMRI(:,7), ...
%             target_left, target_right, data.list_taskfMRI(:,9),data.list_taskfMRI(:,10));
%         T3.Properties.VariableNames = {'subID','runID','runType','trial','RT','ACC','ButtonPress','CorrectButtonPress','Feedback', ...
%             'switch_trial','task','congruency','left_item','task_ifletter','task_ifdigit', ...
%             'target_left','target_right','moneyreward','moneysymbol'};
%         
%         % concatentate subject data with grand table
%         T_trialdata = vertcat(T_trialdata,T3);
%     end

% IN THE SECOND FORLOOP
%     % Baseline 2 (if subject completed the second baseline)
%     if ismember(base2_complete,subjects{i}) == 1
%         
%         %load subject data for run15 (baseline 2)
%         if str2double(subjects{i})<10
%             load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s0',subjects{i},'/LF_s0',subjects{i},'_session4_run15_mixed.mat']);
%         else
%             load(['/Users/debbieyee/Dropbox/CCPLabProjects/LiquidFeedback_FMRI/Data/Behavioral/s',subjects{i},'/LF_s',subjects{i},'_session4_run15_mixed.mat']);
%         end
%         
%         %identify relevant variables for stimulus onsets in incentive runs
%         sid=cellstr(repmat(subjects{i},292,1));
%         runID=repmat(num2cell(15),292,1);
%         runType=repmat(cellstr('baseline'),292,1);
%         tmp_onsets = data.onsets(1:292,:);
%         emptycells = cellfun(@isempty,tmp_onsets);
%         tmp_onsets(emptycells) = {NaN};
%         
%         % create table for baseline run
%         T6 = table(sid,runID,runType,tmp_onsets(:,1),tmp_onsets(:,2),tmp_onsets(:,3),tmp_onsets(:,4),tmp_onsets(:,5),tmp_onsets(:,6));
%         T6.Properties.VariableNames = {'subID','runID','runType','trial','stim_present','onset_calc','onset_exp','comp_onset_calc','comp_onset_exp'};
%         
%         % concatentate subject data with grand table
%         T_stimdata = vertcat(T_stimdata,T6);
%         
%     end
    

end