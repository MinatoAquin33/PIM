# Demography
vars <- c("Demo_L","P_Demo", "Demo_J", "Demo_H", "Demo_I", "Demo_G", "Demo_F", "Demo_E", "Demo_D", "Demo_C", "Demo_B", "Demo")

Demography <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

Demography <- plyr::join_all(
  Demography,
  by = 'SEQN',
  type = 'full'
)



# BMI
vars <- c("BMX_L", "P_BMX", "BMX_J", "BMX_H", "BMX_I", "BMX_G", "BMX_F", "BMX_E", "BMX_D", "BMX_C", "BMX_B", "BMX")

BMX <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

BMX <- plyr::join_all(
  BMX,
  by = 'SEQN',
  type = 'full'
)


# Cotnal
vars <- c("P_COT", "COT_J", "COT_I", "COT_H", "COTNAL_G", "COTNAL_F", "COTNAL_E", "COT_D", "L06COT_C", "L06_B", "LAB06")

COTNAL <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

COTNAL <- plyr::join_all(
  COTNAL,
  by = 'SEQN',
  type = 'full'
)
  

# Alcohol
vars <- c("ALQ_L", "P_ALQ", "ALQ_J", "ALQ_I", "ALQ_H", "ALQ_G", "ALQ_F", "ALQ_E", "ALQ_D", "ALQ_C", "ALQ_B", "ALQ")

ALQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

ALQ <- plyr::join_all(
  ALQ,
  by = 'SEQN',
  type = 'full'
)


# lipids

vars <- c("TCHOL_L", "P_TCHOL", "TCHOL_J", "TCHOL_I", "TCHOL_H", "TCHOL_G", "TCHOL_F", "TCHOL_E", "TCHOL_D", "L13_C", "L13_B", "LAB13")
TC <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

TC <- plyr::join_all(TC, by = 'SEQN', type = 'full')

TC <- TC[, c("SEQN", "LBXTC")]
TC$TC <- TC$LBXTC
TC$LBXTC <- NULL



vars <- c("P_TRIGLY", "TRIGLY_J", "TRIGLY_I", "TRIGLY_H", "TRIGLY_G", "TRIGLY_F", "TRIGLY_E", "TRIGLY_D", "L13AM_C", "L13AM_B", "LAB13AM")
TG <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

TG <- plyr::join_all(TG, by = 'SEQN', type = 'full')

TG <- TG[, c("SEQN", "LBXTR", "LBDLDL")]
TG$TG <- TG$LBXTR
TG$LBXTR <- NULL
TG$LDL_C <- TG$LBDLDL
TG$LBDLDL <- NULL


vars <- c("HDL_L", "P_HDL", "HDL_J", "HDL_I", "HDL_H", "HDL_G", "HDL_F", "HDL_E", "HDL_D", "l13_c", "L13_B", "LAB13")
HDL <- sapply(vars, nhanesA::nhanes, simplify = FALSE)


HDL <- plyr::join_all(HDL, by = 'SEQN', type = 'full')

HDL <- HDL[, c("SEQN", "LBDHDD","LBDHDL","LBXHDD")]
HDL$HDL_C <- rowSums(HDL[, c("LBDHDD", "LBDHDL", "LBXHDD")], na.rm = TRUE)
HDL$HDL_C[apply(HDL[, c("LBDHDD", "LBDHDL", "LBXHDD")], 1, function(x) all(is.na(x)))] <- NA

HDL <- HDL[, c("SEQN", "HDL_C")]



Lipids <- plyr::join_all(list(TG, TC, HDL), by = 'SEQN', type = 'full')
rm(TG, TC, HDL)



# CBC
vars <- c("CBC_L", "P_CBC", "CBC_J", "CBC_I", "CBC_H", "CBC_G", "CBC_F", "CBC_E", "CBC_D", "L25_C", "L25_B", "LAB25")

CBC <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

CBC <- plyr::join_all(
  CBC,
  by = 'SEQN',
  type = 'full'
)

# OGTT
vars <- c("OGTT_I", "OGTT_H", "OGTT_G", "OGTT_F", "OGTT_E", "OGTT_D")

OGTT <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

OGTT <- plyr::join_all(
  OGTT,
  by = 'SEQN',
  type = 'full'
)

# Glucose
vars <- c("GLU_L", "P_GLU", "GLU_J", "GLU_I", "GLU_H", "GLU_G", "GLU_F", "GLU_E", "GLU_D", "L10AM_C", "L10AM_B", "LAB10AM")

GLU <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

GLU <- plyr::join_all(
  GLU,
  by = 'SEQN',
  type = 'full'
)

# Blood Pressure
vars <- c("BPXO_L", "P_BPXO", "BPX_J", "BPX_I", "BPX_H", "BPX_G", "BPX_F", "BPX_E", "BPX_D", "BPX_C", "BPX_B", "BPX")

BPX <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

BPX <- plyr::join_all(
  BPX,
  by = 'SEQN',
  type = 'full'
)

# Blood Pressure Questionaire
vars <- c("BPQ_L", "P_BPQ", "BPQ_J", "BPQ_I", "BPQ_H", "BPQ_G", "BPQ_F", "BPQ_E", "BPQ_D", "BPQ_C", "BPQ_B", "BPQ")

BPQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

BPQ <- plyr::join_all(
  BPQ,
  by = 'SEQN',
  type = 'full'
)

# Diabete Questionaire
vars <- c("DIQ_L", "P_DIQ", "DIQ_J", "DIQ_I", "DIQ_H", "DIQ_G", "DIQ_F", "DIQ_E", "DIQ_D", "DIQ_C", "DIQ_B", "DIQ")

DIQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

DIQ <- plyr::join_all(
  DIQ,
  by = 'SEQN',
  type = 'full'
)

# Osteoporosis
vars <- c("P_OSQ", "OSQ_J", "OSQ_H", "OSQ_F", "OSQ_E", "OSQ_D", "OSQ_C", "OSQ_B", "OSQ")

OSQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

OSQ <- plyr::join_all(
  OSQ,
  by = 'SEQN',
  type = 'full'
)

# Medical Conditions 
vars <- c("MCQ_L", "P_MCQ", "MCQ_J", "MCQ_I", "MCQ_H", "MCQ_G", "MCQ_F", "MCQ_E", "MCQ_D", "MCQ_C", "MCQ_B", "MCQ")

MCQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

MCQ <- plyr::join_all(
  MCQ,
  by = 'SEQN',
  type = 'full'
)

# Physical Activity
vars <- c("PAQ_L", "P_PAQ", "PAQ_J", "PAQ_I", "PAQ_H", "PAQ_G", "PAQ_F", "PAQ_E", "PAQ_D", "PAQ_C", "PAQ_B", "PAQ")

PAQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

PAQ <- plyr::join_all(
  PAQ,
  by = 'SEQN',
  type = 'full'
)

# Smoking
vars <- c("SMQ_L", "P_SMQ", "SMQ_J", "SMQ_I", "SMQ_H", "SMQ_G", "SMQ_F", "SMQ_E", "SMQ_D", "SMQ_C", "SMQ_B", "SMQ")

SMQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

SMQ <- plyr::join_all(
  SMQ,
  by = 'SEQN',
  type = 'full'
)


# Glycohemoglobin
vars <- c("GHB_L", "P_GHB", "GHB_J", "GHB_I", "GHB_H", "GHB_G", "GHB_F", "GHB_E", "GHB_D", "L10_C", "L10_B", "LAB10")

GHB <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

GHB <- plyr::join_all(
  GHB,
  by = 'SEQN',
  type = 'full'
)

# Sleep Disorders
vars <- c("SLQ_L", "P_SLQ", "SLQ_J", "SLQ_I", "SLQ_H", "SLQ_G", "SLQ_F", "SLQ_E", "SLQ_D")

SLQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

SLQ <- plyr::join_all(
  SLQ,
  by = 'SEQN',
  type = 'full'
)

# Depression
vars <- c("DPQ_L", "P_DPQ", "DPQ_J", "DPQ_I", "DPQ_H", "DPQ_G", "DPQ_F", "DPQ_E", "DPQ_D", "CIQDEP_C", "CIQDEP_B", "CIQMDEP")

DPQ <- sapply(vars, nhanesA::nhanes, simplify = FALSE)

DPQ <- plyr::join_all(
  DPQ,
  by = 'SEQN',
  type = 'full'
)

dataset_NHANES <- plyr::join_all(
  list(ALQ,BMX,BPX,CBC,COTNAL,Demography,DIQ,DPQ,GHB,GLU,Lipids,MCQ,OGTT,OSQ,PAQ,SLQ,SMQ,BPQ),
  by = 'SEQN',
  type = 'full'
)
