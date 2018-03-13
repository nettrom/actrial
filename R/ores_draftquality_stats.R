## Investigation into data on the not-OK classes from the ORES draft quality model

library(data.table);
library(ggplot2);

ores_data = fread('datasets/ores_draftquality_stats.tsv');

ggplot() + 
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=n_f1,
            colour='!F1')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=n_precision,
            colour='!Precision')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=n_recall,
            colour='!Recall')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=accuracy,
            colour='Accuracy')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=f1,
            colour='F1')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=filter_rate,
            colour='Filter rate')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=fpr,
            colour='FPR')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=match_rate,
            colour='Match rate')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=precision,
            colour='Precision')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=recall,
            colour='Recall')) +
  geom_line(data=ores_data,
            aes(x=n_recall_given, y=threshold,
            colour='Threshold')) +
  scale_color_manual(values=c(
    `!F1`=cbbPalette[1],
    `!Precision`=cbbPalette[2],
    `!Recall`=cbbPalette[3],
    `Accuracy`=cbbPalette[4],
    `F1`=cbbPalette[5],
    `Filter rate`=cbbPalette[6],
    `FPR`=cbbPalette[7],
    `Match rate`=cbbPalette[8],
    `Precision`='#cccccc',
    `Recall`='#999999',
    `Threshold`='#666666')) +
  guides(colour=guide_legend(title='Statistic')) +
  xlab('!Recall given') + ylab('') +
  ggtitle('ORES enwiki draft quality model statistics for OK class') +
  scale_y_continuous(limits=c(0,1), breaks=c(0:5/5));
