setwd('Downloads')
student = read.csv('student_data (1).csv')
evals = read.csv('evals_data (1).csv')

head(student)
head(evals)

df_new = merge(student, evals, by = 'student_id')

head(df_new)
colnames(df_new)

intake = subset(df_new, df_new$program == 'intake')

a =aggregate(score_reading ~ district, intake, mean)
b = aggregate(score_writing ~ district, intake, mean)
c = aggregate(score_mathNoCalc ~ district, intake, mean)
d =aggregate(score_mathCalc ~ district, intake, mean)

ab = merge(a,b,by = 'district')
cd =merge(c,d, by = 'district')
intake_mean = merge(ab,cd, by = 'district')


df_new$index = NA
df_new$index = 1

for (i in 2:nrow(df_new)){
    if(df_new$student_id[i] == df_new$student_id[i-1]){
        df_new$index[i]=df_new$index[i-1]+1
    }
}



tutoring_second = subset(df_new, df_new$program == 'tutoring' & df_new$index==2)
tutoring_first = subset(df_new, df_new$program == 'tutoring' & df_new$index==1)

m =aggregate(score_reading ~ district, tutoring_second, mean)
n = aggregate(score_writing ~ district, tutoring_second, mean)
o = aggregate(score_mathNoCalc ~ district, tutoring_second, mean)
p =aggregate(score_mathCalc ~ district, tutoring_second, mean)

mn = merge(m,n,by = 'district')
op =merge(o,p, by = 'district')
tutoring_two_mean = merge(mn,op, by = 'district')

skills_second = subset(df_new, df_new$program == 'skills' & df_new$index==2)

w =aggregate(score_reading ~ district, skills_second, mean)
x = aggregate(score_writing ~ district, skills_second, mean)
y = aggregate(score_mathNoCalc ~ district, skills_second, mean)
z =aggregate(score_mathCalc ~ district, skills_second, mean)

wx = merge(w,x,by = 'district')
yz =merge(y,z, by = 'district')
skills_two_mean = merge(wx,yz, by = 'district')

refresh_second = subset(df_new, df_new$program == 'refresh' & df_new$index==2)

q =aggregate(score_reading ~ district, refresh_second, mean)
r = aggregate(score_writing ~ district, refresh_second, mean)
s = aggregate(score_mathNoCalc ~ district, refresh_second, mean)
t =aggregate(score_mathCalc ~ district, refresh_second, mean)

qr = merge(q,r,by = 'district')
st =merge(s,t, by = 'district')
refresh_two_mean = merge(qr,st, by = 'district')

max(df_new$index)


#subset the data frames into online and center
online <- subset(df_new, df_new$location == 'online')
center<- subset(df_new,df_new$location == 'center')

#show breakdown of online scores by program
online_programs_r <- aggregate(score_reading~program,online,mean)
online_programs_w<-aggregate(score_writing~program,online,mean)
online_programs_nc <- aggregate(score_mathNoCalc~program,online,mean)
online_programs_mc<- aggregate(score_mathCalc~program,online,mean)

op1 <- merge(online_programs_r,online_programs_w )
op2 <- merge(online_programs_nc,online_programs_mc)
online_program_scores <- merge(op1,op2)

#breakdown of center scores by program
center_programs_r<- aggregate(score_reading~program,center,mean)
center_programs_w<-aggregate(score_writing~program,center,mean)
center_programs_nc<- aggregate(score_mathNoCalc~program,center,mean)
center_programs_mc<- aggregate(score_mathCalc~program,center,mean)

cp1 <- merge(center_programs_r,center_programs_w )
cp2 <- merge(center_programs_nc,center_programs_mc)
center_program_scores <- merge(cp1,cp2)


#average online scores by subject
online_reading<- mean(online$score_reading)
online_writing<-mean(online$score_writing)
online_nocalc<-mean(online$score_mathNoCalc)
online_calc<-mean(online$score_mathCalc)


#average center scores by subject
center_read <- mean(center$score_reading)
center_write<-mean(center$score_writing)
center_nocalc<-mean(center$score_mathNoCalc)
center_calc<-mean(center$score_mathCalc)



