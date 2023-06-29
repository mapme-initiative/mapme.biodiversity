WORK_DIR=/home/onyxia/work/mapme.biodiversity

git clone https://${GIT_PERSONAL_ACCESS_TOKEN}@github.com/mapme-initiative/mapme.biodiversity.git
chown -R onyxia:users $WORK_DIR

echo \
    "
    setHook('rstudio.sessionInit', function(newSession) {
        if (newSession && !identical(getwd(), \"'$WORK_DIR'\"))
        {
            message('On charge directement le bon projet :-) ')
            rstudioapi::openProject('$WORK_DIR')
            # For a slick dark theme
            rstudioapi::applyTheme('Merbivore')
            # Console where it should be
            rstudioapi::executeCommand('layoutConsoleOnRight')
            # To free the CTRL+Y shortcut for 'redo'
            }
            }, action = 'append')
            " >> /home/onyxia/work/.Rprofile
