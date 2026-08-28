package com.app.meals

import android.app.Application
import com.app.meals.data.AppContainer
import com.app.meals.data.DefaultAppContainer

class MealsApplication : Application() {
    /** AppContainer instance used by the rest of classes to obtain dependencies */
    lateinit var container: AppContainer
    override fun onCreate() {
        super.onCreate()
        container = DefaultAppContainer()
    }
}