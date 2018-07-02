%cd Dropbox\USA\Pittsburgh\GitHub\reversal
load rev_data.mat

ID_list = rev_struct.ID;
ID_list(ID_list == 0) = [];

xlswrite('rev_ID', ID_list)


%convert structure to long-format dataset

revds.specs = struct2dataset(rev_struct.specs);
revds.prob_switch = struct2dataset(rev_struct.prob_switch);
revds.ID = rev_struct.ID;
revds.spont_switch = rev_struct.spont_switch;
revds.persev_error = rev_struct.persev_error;

revds.specs.ID = revds.specs.stim_choice;
revds.specs.trial = revds.specs.stim_choice;
for i = 1: length(revds.ID)
    revds.specs.ID(i) = {ones(length(revds.specs{i,1}),1).*revds.ID(i)};
    revds.specs.trial(i) = {(1:length(revds.specs{i,1}))'};
end

revds.specs = {cell2mat(revds.specs.ID), cell2mat(revds.specs.trial), cell2mat(revds.specs.stim_choice), cell2mat(revds.specs.RT), cell2mat(revds.specs.stim1pos), cell2mat(revds.specs.stim2pos)};    
    
revds.specs = cell2mat(revds.specs);

revds_long = array2table(revds.specs, 'VariableNames', {'ID', 'trial', 'stim_choice','RT', 'stim1pos', 'stim2pos'});

writetable(revds_long, 'rev_long.csv', 'Delimiter', ';')

revds_short = [revds.ID, revds.prob_switch.total, revds.prob_switch.pre_reversal, revds.prob_switch.post_reversal, revds.spont_switch, revds.persev_error];

revds_short = array2table(revds_short, 'VariableNames', {'ID', 'prob_switch_total','prob_switch_pre_rev', 'prob_switch_post_rev', 'spont_switch', 'persev_error'});

writetable(revds_short, 'rev_short.csv', 'Delimiter', ';')